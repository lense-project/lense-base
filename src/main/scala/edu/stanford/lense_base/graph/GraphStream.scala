package edu.stanford.lense_base.graph

import java.util
import java.util.concurrent.locks.{ReentrantReadWriteLock, ReadWriteLock}

import cc.factorie.infer._
import cc.factorie.la
import cc.factorie.la._
import cc.factorie.model._
import cc.factorie.optimize._
import cc.factorie.util.{DoubleSeq, Logger}
import cc.factorie.variable.{CategoricalVectorDomain, FeatureVectorVariable, _}
import edu.stanford.lense_base.util.CaseClassEq

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * Created by keenon on 4/20/15.
 *
 * Holds the means to specify the components necessary to handle PGM problems in real time, without most of the code
 * grit associated with using FACTORIE directly (though no one denies it is tremendously powerful for direct use, just
 * hard to learn, and some things (like attaching features to a hyper-arc) have awkward structure)
 */

// GraphVarWithDomain is an easy way to refer to both Node and Factor (declared in Graph), since both will need to be
// represented in the FACTORIE graph partially by a node which holds the feature values.

abstract class GraphVarWithDomain(initDomainType : WithDomain) {
  def domainType = initDomainType
  def features : Map[String, Double]
}

case class GraphNode(graph : Graph,
                      nodeType: NodeType,
                      var features: Map[String, Double] = null,
                      var observedValue: String = null,
                      payload: Any = null,
                      stringName : String = null) extends GraphVarWithDomain(nodeType) with CaseClassEq {

  val id: Int = graph.nodes.size
  graph.nodes += this

  var variable = NodeVariable(this, graph)

  var numHumanObservations : Int = 0

  if (features == null) features = Map("BIAS" -> 1.0)
  else if (!features.contains("BIAS")) features = features ++ Map("BIAS" -> 1.0)

  override def hashCode() : Int = nodeType.hashCode() + id.hashCode()

  override def toString : String = if (stringName != null) stringName else super.toString
}

case class GraphFactor(graph : Graph,
                        factorType: FactorType,
                        nodes: Iterable[GraphNode],
                        var features: Map[String, Double] = null,
                        payload: Any = null,
                        stringName : String = null) extends GraphVarWithDomain(factorType) with CaseClassEq {

  // A type check verification at initialization
  // to prevent initialization with mismatched nodes and factorType
  val nodeList : List[GraphNode] = nodes.toList
  if (nodeList.size != factorType.neighborTypes.size) throw new IllegalArgumentException("Wrong number of neighbors"
  +" for this factorType. The factorType expects neighbors of type (and in order) "+factorType.neighborTypes+", and"
  +" you gave nodes with type "+nodes.map(_.nodeType).toString)
  for (i <- 0 to nodeList.size-1) {
    if (nodeList(i).nodeType != factorType.neighborTypes(i)) throw new IllegalArgumentException("Created a Factor "
    +"with the wrong neighbor type at (atleast, but we haven't checked higher indices yet) neighbor #"+i+", where "+
    "the node type is "+nodeList(i).nodeType+" and expected type is "+factorType.neighborTypes(i))
  }

  graph.factors += this

  if (features == null) features = Map("BIAS" -> 1.0)
  else if (!features.contains("BIAS")) features = features ++ Map("BIAS" -> 1.0)

  override def hashCode() : Int = factorType.hashCode() + nodes.hashCode()

  override def toString : String = if (stringName != null) "GraphFactor("+stringName+")" else super.toString
}

case class Graph(stream : GraphStream, overrideToString : String = null) extends CaseClassEq {
  val nodes: mutable.MutableList[GraphNode] = new mutable.MutableList[GraphNode]()
  val factors: mutable.MutableList[GraphFactor] = new mutable.MutableList[GraphFactor]()

  override def toString : String = if (overrideToString != null) overrideToString else super.toString

  override def clone() : (Graph, Map[GraphNode, GraphNode]) = {
    val g = stream.newGraph()

    // Double check for corruption
    for (factor <- factors) {
      for (node <- factor.nodes) {
        if (!nodes.contains(node)) throw new IllegalStateException("Corrupt state, contain nodes we're not supposed to")
      }
    }

    val oldToNew : Map[GraphNode, GraphNode] = nodes.map{node => {
      (node, g.makeNode(node.nodeType, node.features, node.observedValue, node.payload, node.toString))
    }}.toMap
    for (factor <- factors) {
      g.makeFactor(factor.factorType, factor.nodes.map(oldToNew(_)), factor.features)
    }
    (g, oldToNew)
  }

  def makeNode(nodeType : NodeType, features : Map[String,Double] = null, observedValue: String = null, payload : Any = null, toString : String = null) : GraphNode = {
    GraphNode(this, nodeType, features, observedValue, payload, toString)
  }

  def makeFactor(factorType : FactorType, nodes : Iterable[GraphNode], features : Map[String,Double] = null, payload : Any = null, toString : String = null) : GraphFactor = {
    for (node <- nodes) {
      if (!this.nodes.contains(node)) throw new IllegalStateException()
    }
    GraphFactor(this, factorType, nodes, features, payload, toString)
  }

  def mapEstimate(model : GraphStream#StreamModel = stream.model): Map[GraphNode, String] = {
    setObservedVariablesForFactorie()
    val variables = unobservedVariablesForFactorie()
    if (variables.size == 0) return Map()
    model.synchronized {
      model.warmUpIndexes(this)
    }

    ////////////////////////////
    // SYNCHRONIZED SECTION
    stream.weightsReadWriteLock.readLock().lock()

    // Do exact inference via trees if possible.
    val sumMax = try {
      MaximizeByBPTree.infer(variables, model)
    }
    // If that fails, perform loopy maximization
    catch {
      case e : Throwable =>
        e.printStackTrace()
        // run loopy bp
        println("MAP Inference is falling back to loopy BP. Results may be inexact")
        MaximizeByBPLoopyTreewise.infer(variables, model)
    }

    stream.weightsReadWriteLock.readLock().unlock()
    // END SYNCHRONIZED SECTION
    ////////////////////////////

    variables.filter(_.node.observedValue == null).map{
      case nodeVar : NodeVariable =>
        nodeVar.node -> (sumMax.getMarginal(nodeVar).get match {
          case dm : DiscreteMarginal1[NodeVariable] =>
            dm.value1.category
          case sm : MAPSummary#SingletonMarginal =>
            sm.setToMaximize(null)
            nodeVar.categoryValue
        })
    }.toMap
  }

  def factorsMarginalEstimate(model : GraphStream#StreamModel = stream.model, ignoreObservedValues : Boolean = false): Map[GraphFactor, Map[List[String],Double]] = {
    setObservedVariablesForFactorie()
    val variables = if (ignoreObservedValues) allVariablesForFactorie() else unobservedVariablesForFactorie()
    if (variables.size == 0) return Map()
    model.synchronized {
      model.warmUpIndexes(this)
    }

    ////////////////////////////
    // SYNCHRONIZED SECTION
    stream.weightsReadWriteLock.readLock().lock()

    // Do exact inference via trees if possible.
    val sumMarginal = try {
      InferByBPTree.infer(variables, model)
    }
    // If that fails, perform gibbs sampling
    catch {
      case _ : Throwable =>
        /*

        It seems the Factorie Gibbs Sampler is very slow, creating tons of unnecessary objects to perform sampling.
        So we don't use it.

        // val r = new scala.util.Random(42)
        // randomly initialize to valid values
        // for (variable <- variables) variable.set(r.nextInt(variable.domain.size))(null)
        // run gibbs sampler
        // new InferByGibbsSampling(samplesToCollect = 100, 10, r).infer(variables, stream.model)
        */
        InferByBPLoopyTreewise.infer(variables, model)
    }

    stream.weightsReadWriteLock.readLock().unlock()
    // END SYNCHRONIZED SECTION
    ////////////////////////////

    sumMarginal.factorMarginals.map(f => {
      // Get all corresponding Factors in the GraphStream sense
      (f, factors.filter(factor => {
        val featureVariable = model.getFeatureVariableFor(factor, null)
        val variables = factor.nodeList.map(_.variable)
        val factorVariables = f.factor.variables.slice(0, f.factor.variables.size-1)
        factorVariables == variables
      }))
    })
    // Only look at marginals who have exactly one corresponding factor
    .filter(_._2.size == 1)
    // Map to the (Factor,tensorStatistics) pair from the marginals
    .map(pair => (pair._2.head, {
      pair._1.asInstanceOf[DiscreteMarginal].proportions
    }))
    // Map raw tensor marginals to a map of key->value
    .map(pair => {
      val factor = pair._1
      val tensor = pair._2
      val tensorArray = tensor.toArray

      val marginals = factor.nodeList.size match {
        case 1 =>
          throw new IllegalStateException("Not yet supported")
        case 2 =>
          val domainSize = factor.nodeList(0).nodeType.valueDomain.size

          val node1 = factor.nodeList(0)
          val node2 = factor.nodeList(1)

          node1.nodeType.valueDomain.categories.flatMap(cat1 => {
            val i1 = node1.nodeType.valueDomain.index(cat1)

            node2.nodeType.valueDomain.categories.map(cat2 => {
              val i2 = node2.nodeType.valueDomain.index(cat2)

              val key = List(cat1, cat2)

              // If we're observing values, the proportions array is shortened to just include marginals over un-observed values
              // so we need some ugly logic to pick apart all the marginals
              val v = if (!ignoreObservedValues) {
                if (node1.observedValue == cat1 && node2.observedValue == null) {
                  tensorArray(i2)
                }
                else if (node2.observedValue == cat2 && node1.observedValue == null) {
                  tensorArray(i1)
                }
                else if (node1.observedValue == cat1 && node2.observedValue == cat2) {
                  1.0
                }
                else if (node1.observedValue != null && node1.observedValue != cat1) {
                  0.0
                }
                else if (node2.observedValue != null && node2.observedValue != cat2) {
                  0.0
                }
                else {
                  tensorArray((i1*domainSize) + i2)
                }
              }
              // If we're ignoring observed values, don't bother with zeroing out or remapping, cause the proportions will
              // be exactly the right size
              else {
                tensorArray((i1 * domainSize) + i2)
              }

              // key value pair
              key -> v
            })
          }).toMap
        case 3 =>
          throw new IllegalStateException("Not yet supported")
      }
      (factor, marginals)
    }).toMap
  }

  def logLikelihoodWithUnobserved(model : GraphStream#StreamModel = stream.model): Double = {
    setObservedVariablesForFactorie()
    val unobservedVariables = unobservedVariablesForFactorie()
    if (unobservedVariables.size == 0) return 0.0
    val allVariables = allVariablesForFactorie()
    if (allVariables.size == 0) return 0.0

    model.synchronized {
      model.warmUpIndexes(this)
    }

    ////////////////////////////
    // SYNCHRONIZED SECTION
    stream.weightsReadWriteLock.readLock().lock()

    // Do exact inference via trees if possible.
    val sumMarginalObserved = try {
      InferByBPTree.infer(unobservedVariables, model)
    }
    // If that fails, perform gibbs sampling
    catch {
      case _ : Throwable =>
        InferByBPLoopyTreewise.infer(unobservedVariables, model)
    }

    val sumMarginalUnobserved = try {
      InferByBPTree.infer(allVariables, model)
    }
    // If that fails, perform gibbs sampling
    catch {
      case _ : Throwable =>
        InferByBPLoopyTreewise.infer(allVariables, model)
    }

    stream.weightsReadWriteLock.readLock().unlock()
    // END SYNCHRONIZED SECTION
    ////////////////////////////

    /*
    println("Unobserved log-Z: "+sumMarginalUnobserved.logZ)
    println("Partially observed log-Z: "+sumMarginalObserved.logZ)
    */

    sumMarginalObserved.logZ - sumMarginalUnobserved.logZ
  }

  def marginalEstimate(model : GraphStream#StreamModel = stream.model, ignoreObservedValues : Boolean = false): Map[GraphNode, Map[String,Double]] = {
    setObservedVariablesForFactorie()
    val variables = if (ignoreObservedValues) allVariablesForFactorie() else unobservedVariablesForFactorie()
    if (variables.size == 0) return Map()

    model.synchronized {
      model.warmUpIndexes(this)
    }

    ////////////////////////////
    // SYNCHRONIZED SECTION
    stream.weightsReadWriteLock.readLock().lock()

    // Do exact inference via trees if possible.
    val sumMarginal = try {
      InferByBPTree.infer(variables, model)
    }
    // If that fails, perform gibbs sampling
    catch {
      case _ : Throwable =>
        /*

        It seems the Factorie Gibbs Sampler is very slow, creating tons of unnecessary objects to perform sampling.
        So we don't use it.

        // val r = new scala.util.Random(42)
        // randomly initialize to valid values
        // for (variable <- variables) variable.set(r.nextInt(variable.domain.size))(null)
        // run gibbs sampler
        // new InferByGibbsSampling(samplesToCollect = 100, 10, r).infer(variables, stream.model)
        */
        InferByBPLoopyTreewise.infer(variables, model)
    }

    stream.weightsReadWriteLock.readLock().unlock()
    // END SYNCHRONIZED SECTION
    ////////////////////////////

    variables.filter(_.node.observedValue == null).map{
      case nodeVar : NodeVariable =>
        // Create node -> [value,score] pair for map
        nodeVar.node -> {
          val props : Array[Double] = sumMarginal.getMarginal(nodeVar).get.asInstanceOf[DiscreteMarginal1[NodeVariable]].proportions.toArray
          props.zipWithIndex.map(scoreIndexPair => {
            nodeVar.node.nodeType.valueDomain.category(scoreIndexPair._2) -> scoreIndexPair._1
          }).toMap
        }
    }.toMap
  }

  def unobservedVariablesForFactorie(): Seq[NodeVariable] = {
    nodes.filter(_.observedValue == null).map(_.variable)
  }

  def setObservedVariablesForFactorie(): Unit = {
    nodes.filter(_.observedValue != null).foreach(n => {
      n.variable.set(n.nodeType.valueDomain.index(n.observedValue))(null)
    })
  }

  def allVariablesForFactorie(): Seq[NodeVariable] = {
    nodes.map(_.variable)
  }
}

abstract class WithDomain(stream : GraphStream) {
  val featureDomain = new CategoricalDomain[String] {
    // This is a slight optimization to make training 40% faster, by speeding up frozen domain map lookups
    override def hashCode() : Int = {
      _elements.size
    }
  }
  val domain = new CategoricalVectorDomain[String] { override def dimensionDomain = featureDomain }
  stream.withDomainList.synchronized {
    stream.withDomainList += this
  }

  def getWeights : Map[Any, Map[String,Double]]

  def getExpNormalizedWeights : Map[Any, Map[String,Double]] = {
    val rawWeights = getWeights
    val expWeights = rawWeights.map(pair => (pair._1, pair._2.map(q => (q._1, Math.exp(q._2)))))
    val sum = expWeights.flatMap(_._2.map(_._2)).sum
    expWeights.map(pair => (pair._1, pair._2.map(q => (q._1, q._2 / sum))))
  }

  def setWeights(newWeights : Map[Any, Map[String,Double]])

  def abstractPossibleValues : Set[Any]
}

case class NodeType(stream : GraphStream, possibleValues : Set[String], var weights : Map[String,Map[String,Double]] = null) extends WithDomain(stream) with CaseClassEq {
  val valueDomain = new CategoricalDomain[String]
  valueDomain.indexAll(possibleValues)

  override def hashCode : Int = possibleValues.hashCode()

  def setWeights(newWeights : Map[Any,Map[String,Double]]) = {
    weights = newWeights.asInstanceOf[Map[String,Map[String,Double]]]
    stream.model.synchronized {
      stream.model.dotFamilyCache.remove(this)
    }
  }

  def abstractPossibleValues : Set[Any] = possibleValues.asInstanceOf[Set[Any]]

  override def getWeights: Map[Any, Map[String, Double]] = weights.asInstanceOf[Map[Any, Map[String, Double]]]
}

case class FactorType(stream : GraphStream, neighborTypes : List[NodeType], var weights : Map[List[String],Map[String,Double]] = null) extends WithDomain(stream) with CaseClassEq {
  if (neighborTypes.size < 1 || neighborTypes.size > 3)
    throw new UnsupportedOperationException("FactorType doesn't support neighbor lists smaller than 1, or larger "+
      "than 3, due to underlying design decisions in FACTORIE making larger factor support a total pain in the ass.")

  override def hashCode : Int = neighborTypes.hashCode()

  def possibleValues : Set[List[String]] = {
    if (neighborTypes.size == 1) {
      neighborTypes(0).possibleValues.map(v => List(v))
    }
    else if (neighborTypes.size == 2) {
      neighborTypes(0).possibleValues.flatMap(v1 => {
        neighborTypes(1).possibleValues.map(v2 => {
          List(v1,v2)
        })
      })
    }
    else if (neighborTypes.size == 3) {
      neighborTypes(0).possibleValues.flatMap(v1 => {
        neighborTypes(1).possibleValues.flatMap(v2 => {
          neighborTypes(2).possibleValues.map(v3 => {
            List(v1, v2, v3)
          })
        })
      })
    }
    else {
      throw new IllegalStateException("Factor has the wrong number of neighbors")
    }
  }

  def setWeights(newWeights : Map[Any,Map[String,Double]]) = {
    weights = newWeights.asInstanceOf[Map[List[String],Map[String,Double]]]
    stream.model.synchronized {
      stream.model.dotFamilyCache.remove(this)
    }
  }

  override def getWeights: Map[Any, Map[String, Double]] = weights.asInstanceOf[Map[Any, Map[String, Double]]]

  def abstractPossibleValues : Set[Any] = possibleValues.asInstanceOf[Set[Any]]
}

// This holds the Multi-class decision variable component of a given node.
// The model expects a list of these to be passed in.

case class NodeVariable(node : GraphNode, graph : Graph) extends CategoricalVariable[String] with LabeledMutableCategoricalVar[String] {
  var frozenDomainMap : Map[CategoricalDomain[String], CategoricalDomain[String]] = null
  override lazy val domain = if (frozenDomainMap == null || !frozenDomainMap.contains(node.nodeType.valueDomain))
    node.nodeType.valueDomain
  else frozenDomainMap(node.nodeType.valueDomain)

  override type TargetType = CategoricalTargetVariable[String]
  override def target: TargetType = new CategoricalTargetVariable[String](domain.index(node.observedValue), this)
}

class GraphStream {

  // NodeType and FactorType are a way to share weights across different nodes.
  // They are assumed to have the same domain (mapping from feature string to int), and they share weights globally.

  val withDomainList = mutable.MutableList[WithDomain]()

  def makeNodeType(possibleValues : Set[String], weights : Map[String,Map[String,Double]] = null) : NodeType = {
    NodeType(this, possibleValues, weights)
  }

  def makeFactorType(neighborTypes : List[NodeType], weights : Map[List[String],Map[String,Double]] = null) : FactorType = {
    FactorType(this, neighborTypes, weights)
  }

  def newGraph(toString : String = null) : Graph = Graph(this, toString)

  def onlineUpdate(graphs : Iterable[Graph], regularization: Double = 0.1, clearOptimizer : Boolean = true) = {
    if (graphs.exists(graph => graph.nodes.exists(node => {
      node.observedValue == null
    }))) onlineEM(graphs, clearOptimizer)
    else onlineUpdateFullyObserved(graphs, regularization, clearOptimizer)
  }

  var confidenceVCorrectnessCache : Map[Int, List[(Map[String,Double],String)]] = Map()

  // learns the appropriate bits, which means any weight factors, and EM if there are any
  // nodes with unobserved values. This is called for its byproducts, and will just go in and update the existing
  // weights on the NodeTypes and FactorTypes that are involved in the graphs that were passed in.

  def learn(graphs : Iterable[Graph],
            l2regularization: Double = 0.1,
            clearOptimizer : Boolean = true) : Double = {
    if (graphs.size == 0) return 0.0

    if (graphs.exists(graph => graph.nodes.exists(node => {
      node.observedValue == null
    }))) learnEM(graphs, l2regularization, clearOptimizer)
    else learnFullyObserved(graphs, l2regularization, clearOptimizer)
  }

  // performs EM on the graph. Still massively TODO

  private def learnEM(graphs : Iterable[Graph], l2regularization : Double, clearOptimizer : Boolean = true) : Double = {
    val nodeTypes = graphs.flatMap(_.nodes.map(_.nodeType)).toList.distinct
    val factorTypes = graphs.flatMap(_.factors.map(_.factorType)).toList.distinct
    val withDomainTypes = nodeTypes ++ factorTypes

    for (graph <- graphs) {
      modelTrainingClone.warmUpIndexes(graph)
    }
    modelTrainingClone.dotFamilyCache.clear()

    val localWithDomainList = graphs.flatMap(g => {
      g.nodes.map(_.nodeType).distinct ++ g.factors.map(_.factorType).distinct
    }).toList.distinct

    val frozenDomainMap = withDomainList.synchronized {
      localWithDomainList.map(withDomain => {
        val newDomain = new CategoricalDomain[String]()
        newDomain.indexAll(withDomain.domain.dimensionDomain.categories.toArray)
        (withDomain.domain.dimensionDomain, newDomain)
      }).toMap
    }

    graphs.map(graph => {
      graph.setObservedVariablesForFactorie()
      val nodeVariables = graph.allVariablesForFactorie()
      for (nodeVariable <- nodeVariables) {
        nodeVariable.frozenDomainMap = frozenDomainMap
      }
      new LikelihoodExample(nodeVariables, modelTrainingClone, InferByBPTree)
    }).toSeq

    var converged = false
    var lastLoss = Double.NegativeInfinity
    var convergenceCounter = 0

    while (!converged) {

      // Create blank weight sets

      val typeAverage = mutable.Map[WithDomain, Map[Any, mutable.Map[String,Double]]]()
      val typeCounts = mutable.Map[WithDomain, Int]()

      // Initialize the weight sets with all zeros

      typeCounts ++= nodeTypes.map(nt => (nt, 0))
      typeCounts ++= factorTypes.map(ft => (ft, 0))

      // Collect average marginals over all graphs

      var logLikelihood = 0.0
      var regularizer = 0.0

      for (graph <- graphs) {
        logLikelihood += graph.logLikelihoodWithUnobserved(modelTrainingClone)

        val marginals = graph.marginalEstimate(modelTrainingClone) ++ graph.factorsMarginalEstimate(modelTrainingClone).asInstanceOf[Map[GraphVarWithDomain, Map[Any, Double]]]
        val unobservedMarginals = graph.marginalEstimate(modelTrainingClone, ignoreObservedValues = true) ++ graph.factorsMarginalEstimate(modelTrainingClone, ignoreObservedValues = true).asInstanceOf[Map[GraphVarWithDomain, Map[Any, Double]]]

        marginals.foreach(pair => {
          val withDomain = pair._1
          val domainType: WithDomain = withDomain match {
            case node: GraphNode => node.nodeType
            case factor: GraphFactor => factor.factorType
          }
          typeCounts.put(domainType, typeCounts(domainType) + 1)

          if (!typeAverage.contains(domainType)) typeAverage.put(domainType, domainType.abstractPossibleValues.map(value => (value, mutable.Map[String, Double]())).toMap)

          val localObservedMarginals: Map[Any, Double] = pair._2.asInstanceOf[Map[Any, Double]]
          val localUnobservedMarginals: Map[Any, Double] = unobservedMarginals(pair._1).asInstanceOf[Map[Any, Double]]

          for (valueWithProb <- localObservedMarginals) {
            val value = valueWithProb._1
            val prob = valueWithProb._2
            val unobservedProb = localUnobservedMarginals(value)

            val mutableWeightsAverageMap = typeAverage(domainType)(value)

            for (featureWithWeight <- withDomain.features) {
              val feature = featureWithWeight._1
              val weight = featureWithWeight._2
              mutableWeightsAverageMap.put(feature, mutableWeightsAverageMap.getOrElse(feature, 0.0) + (prob - unobservedProb) * weight)
            }
          }
        })
      }

      for (withDomain <- withDomainTypes) {
        if (withDomain.getWeights != null) {
          for (w <- withDomain.getWeights) {
            for (q <- w._2) {
              regularizer += q._2 * q._2
            }
          }
        }
      }

      regularizer = - regularizer * l2regularization / 2
      System.err.println("Value: "+logLikelihood)
      System.err.println("Regularizer: "+regularizer)
      System.err.println("Value + Regularizer: "+(logLikelihood + regularizer))
      val loss = logLikelihood + regularizer
      val percentage = (lastLoss - loss) / lastLoss
      System.err.println("percentage improvement: "+percentage)
      if (percentage < 0.001) {
        convergenceCounter += 1
        System.err.println("convergence counter: "+convergenceCounter)
        if (convergenceCounter > 4) converged = true
      }
      lastLoss = loss

      // Done collecting marginals

      for (pair <- typeAverage) {
        val withDomain = pair._1
        val unnormalizedAverage = pair._2
        val normalizedAverage = unnormalizedAverage.map(assignmentAvg => {
          (assignmentAvg._1, assignmentAvg._2.map(q => (q._1, q._2 / typeCounts.getOrElse(withDomain, 1))))
        })

        if (withDomain.getWeights == null) {
          withDomain.setWeights(normalizedAverage.map(assignmentAvg => {
            (assignmentAvg._1, assignmentAvg._2.toMap)
          }))
        }
        else {
          withDomain.setWeights(withDomain.getWeights.map(oldWeightsForSinglePossibleValue => {
            val possibleValue = oldWeightsForSinglePossibleValue._1
            val weightsForPossibleValue = oldWeightsForSinglePossibleValue._2
            (possibleValue, weightsForPossibleValue.map(q => {
              val v = normalizedAverage.getOrElse(possibleValue, Map[String,Double]()).getOrElse(q._1, 0.0)
              val grad = v - (q._2 * l2regularization)
              (q._1, q._2 + grad*0.5)
            }))
          }))
        }
      }

      /*
      System.err.println(nodeTypes)
      System.err.println(factorTypes)
      */

      // Force a regeneration of the dotFamilyCache
      modelTrainingClone.dotFamilyCache.synchronized {
        modelTrainingClone.dotFamilyCache.clear()
      }
    }

    lastLoss
  }

  private def onlineEM(graphs : Iterable[Graph], clearOptimizer : Boolean = true) : Double = {
    throw new UnsupportedOperationException("We don't yet support EM. Make sure all your variables have observed values.")
  }

  var onlineOptimizer : GradientOptimizer = null
  private def onlineUpdateFullyObserved(graphs : Iterable[Graph], regularization : Double, clearOptimizer : Boolean = true): Double = {
    if (onlineOptimizer == null || clearOptimizer) {
      onlineOptimizer = new AdaGrad()
    }
    // Don't want to be doing this part in parallel, things get broken
    for (graph <- graphs) {
      model.warmUpIndexes(graph)
    }

    val likelihoodExamples = graphs.map(graph => {
      graph.setObservedVariablesForFactorie()
      new LikelihoodExample(graph.allVariablesForFactorie(), model, InferByBPChain)
    }).toSeq

    val trainer = new OnlineTrainer(model.parameters, onlineOptimizer, maxIterations = 100)
    trainer.processExamples(likelihoodExamples)

    val value = new SynchronizedDoubleAccumulator()
    for (likelihoodExample <- likelihoodExamples) {
      likelihoodExample.accumulateValueAndGradient(value, null)
    }
    value.l.value
  }

  // This will learn just Weight() values from the fully observed values in the graphs

  var batchOptimizer : GradientOptimizer = null
  private def learnFullyObserved(graphs : Iterable[Graph], l2regularization : Double, clearOptimizer : Boolean = true): Double = {
    val loss = modelTrainingClone.synchronized {
      if (batchOptimizer == null || clearOptimizer) {
        batchOptimizer = new BatchAdaGrad()
      }
      batchOptimizer.asInstanceOf[BatchAdaGrad]._isConverged = false
      batchOptimizer.asInstanceOf[BatchAdaGrad].l2regularization = l2regularization

      val localWithDomainList = graphs.flatMap(g => {
        g.nodes.map(_.nodeType).distinct ++ g.factors.map(_.factorType).distinct
      }).toList.distinct

      // Don't want to be doing this part in parallel, things get broken

      val likelihoodExamples = model.synchronized {
        for (graph <- graphs) {
          model.warmUpIndexes(graph)
          modelTrainingClone.dotFamilyCache.clear()
        }

        val frozenDomainMap = withDomainList.synchronized {
          localWithDomainList.map(withDomain => {
            val newDomain = new CategoricalDomain[String]()
            newDomain.indexAll(withDomain.domain.dimensionDomain.categories.toArray)
            (withDomain.domain.dimensionDomain, newDomain)
          }).toMap
        }

        graphs.map(graph => {
          graph.setObservedVariablesForFactorie()
          val nodeVariables = graph.allVariablesForFactorie()
          for (nodeVariable <- nodeVariables) {
            nodeVariable.frozenDomainMap = frozenDomainMap
          }
          new LikelihoodExample(nodeVariables, modelTrainingClone, InferByBPTree)
        }).toSeq
      }

      // Trainer.batchTrain(model.parameters, likelihoodExamples, optimizer = new ConjugateGradient() with L2Regularization)(new scala.util.Random(42))
      Trainer.batchTrain(modelTrainingClone.parameters, likelihoodExamples, optimizer = batchOptimizer, useParallelTrainer = true, maxIterations = 10000)(new scala.util.Random(42))

      // val trainer = new BatchTrainer(model.parameters, new LBFGS() with L2Regularization{variance = regularization}, maxIterations = 100)
      // trainer.trainFromExamples(likelihoodExamples)

      ////////////////////////////
      // SYNCHRONIZED SECTION
      weightsReadWriteLock.writeLock().lock()

      // Copy over weights from the modelTrainingClone's dotFamilies to the model's dotFamilies
      for (w <- localWithDomainList) {
        val trainedDotFamily = modelTrainingClone.getDotFamilyWithStatisticsFor(w)
        val untrainedDotFamily = model.getDotFamilyWithStatisticsFor(w)
        if (untrainedDotFamily.weights.value.size == trainedDotFamily.weights.value.size) {
          untrainedDotFamily.weights.value := trainedDotFamily.weights.value
        }
        else {
          println("Tensors are different size now")
        }
      }

      weightsReadWriteLock.writeLock().unlock()
      // END SYNCHRONIZED SECTION
      ////////////////////////////

      val value = new SynchronizedDoubleAccumulator()
      for (likelihoodExample <- likelihoodExamples) {
        likelihoodExample.accumulateValueAndGradient(value, null)
      }
      val regularizer = - (modelTrainingClone.parameters.dot(modelTrainingClone.parameters) * l2regularization / 2)
      println("SYSTEM RETURN REGULARIZER: "+regularizer)
      println("SYSTEM RETURN VALUE: "+value.l.value)
      println("SYSTEM RETURN VALUE+REGURLAIZER: "+(value.l.value+regularizer))
      value.l.value + regularizer
    }

    // Now we need to decode the weights

    model.dotFamilyCache.synchronized {
      withDomainList.synchronized {
        for (withDomain <- withDomainList) {
          if (!model.dotFamilyCache.containsKey(withDomain)) {
            // We ignore these, on the assumption that we don't want to return weights full of zeros for no reason
          }
          else {
            val dot: DotFamily = model.getDotFamilyWithStatisticsFor(withDomain)
            val tensor = dot.weights.value
            if (tensor.size > 0) {
              withDomain match {

                // Translate a nodeType's weights back into the Map we use for weights

                case nodeType: NodeType =>
                  if (tensor.dimensions.length != 2) throw new IllegalStateException("Can't have weights for a node unary " +
                    "factor that aren't nodeValues x nodeFeatures, which means dim=2. Instead got dim=" + tensor.dimensions.length)
                  if (tensor.dimensions(0) != nodeType.possibleValues.size) throw new IllegalStateException("Have a set of possibleValues that" +
                    " doesn't match the value domain: " + nodeType.valueDomain.categories + ", " + nodeType.possibleValues)
                  // we need the weight values for each possible assignment
                  val tensor2 = tensor.asInstanceOf[Tensor2]
                  val keyValue: ListBuffer[(String, Map[String, Double])] = ListBuffer()
                  for (val1 <- nodeType.possibleValues) {
                    val index1 = nodeType.valueDomain.index(val1)
                    keyValue += val1 ->
                      (0 to tensor2.dim2 - 1).map(featIndex => {
                        val feature: String = nodeType.featureDomain.category(featIndex)
                        val weight: Double = tensor2(index1, featIndex)
                        feature -> weight
                      }).toMap
                  }
                  nodeType.setWeights(keyValue.toMap)

                // Translate a factorType's weights back into the Map we use for weights

                case factorType: FactorType =>
                  factorType.neighborTypes.size match {
                    case 1 =>
                      if (tensor.dimensions.length != 3) throw new IllegalStateException("Can't have weights for a Factor's " +
                        "factor that isn't val1 x features, which means dim=2. Instead got dim=" + tensor.dimensions.length)
                      if (tensor.dimensions(0) != factorType.neighborTypes(0).possibleValues.size) throw new IllegalStateException()

                      val tensor2 = tensor.asInstanceOf[Tensor2]
                      val keyValue: ListBuffer[(List[String], Map[String, Double])] = ListBuffer()
                      for (val1 <- factorType.neighborTypes(0).possibleValues) {
                        val index1 = factorType.neighborTypes(0).valueDomain.index(val1)
                        keyValue += List(val1) ->
                          (0 to tensor2.dim2 - 1).map(featIndex => {
                            val feature = factorType.featureDomain.category(featIndex)
                            val weight = tensor2(index1, featIndex)
                            feature -> weight
                          }).toMap
                      }
                      factorType.setWeights(keyValue.toMap)
                    case 2 =>
                      if (tensor.dimensions.length != 3) throw new IllegalStateException("Can't have weights for a Factor's " +
                        "factor that isn't val1 x val2 x features, which means dim=3. Instead got dim=" + tensor.dimensions.length)
                      if (tensor.dimensions(0) != factorType.neighborTypes(0).possibleValues.size) throw new IllegalStateException()
                      if (tensor.dimensions(1) != factorType.neighborTypes(1).possibleValues.size) throw new IllegalStateException()
                      if (tensor.dimensions(2) != factorType.featureDomain.size) throw new IllegalStateException()

                      val tensor3 = tensor.asInstanceOf[Tensor3]
                      val keyValue: ListBuffer[(List[String], Map[String, Double])] = ListBuffer()
                      for (val1 <- factorType.neighborTypes(0).possibleValues) {
                        val index1 = factorType.neighborTypes(0).valueDomain.index(val1)
                        for (val2 <- factorType.neighborTypes(1).possibleValues) {
                          val index2 = factorType.neighborTypes(1).valueDomain.index(val2)

                          keyValue += List(val1, val2) ->
                            (0 to tensor3.dim3 - 1).map(featIndex => {
                              val feature = factorType.featureDomain.category(featIndex)
                              val weight = tensor3(index1, index2, featIndex)
                              feature -> weight
                            }).toMap
                        }
                      }
                      factorType.setWeights(keyValue.toMap)
                    case 3 =>
                      if (tensor.dimensions.length != 3) throw new IllegalStateException("Can't have weights for a Factor's " +
                        "factor that isn't val1 x val2 x val3 x features, which means dim=4. Instead got dim=" + tensor.dimensions.length)
                      if (tensor.dimensions(0) != factorType.neighborTypes(0).possibleValues.size) throw new IllegalStateException()
                      if (tensor.dimensions(1) != factorType.neighborTypes(1).possibleValues.size) throw new IllegalStateException()
                      if (tensor.dimensions(2) != factorType.neighborTypes(2).possibleValues.size) throw new IllegalStateException()
                      if (tensor.dimensions(3) != factorType.featureDomain.size) throw new IllegalStateException()

                      val tensor4 = tensor.asInstanceOf[Tensor4]
                      val keyValue: ListBuffer[(List[String], Map[String, Double])] = ListBuffer()
                      for (val1 <- factorType.neighborTypes(0).possibleValues) {
                        val index1 = factorType.neighborTypes(0).valueDomain.index(val1)
                        for (val2 <- factorType.neighborTypes(1).possibleValues) {
                          val index2 = factorType.neighborTypes(1).valueDomain.index(val2)
                          for (val3 <- factorType.neighborTypes(2).possibleValues) {
                            val index3 = factorType.neighborTypes(2).valueDomain.index(val3)

                            keyValue += List(val1, val2, val3) ->
                              (0 to tensor4.dim4 - 1).map(featIndex => {
                                val feature = factorType.featureDomain.category(featIndex)
                                val weight = tensor4(index1, index2, index3, featIndex)
                                feature -> weight
                              }).toMap
                          }
                        }
                      }
                      factorType.setWeights(keyValue.toMap)
                    case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
                  }
              }
            }
          }
        }
      }
    }
    loss
  }


  def getIndexCautious(domain : CategoricalDomain[String], value : String, nameForError : String) : Int = {
    val oldSize = domain.size
    val index = domain.index(value)
    if (oldSize != domain.size)
      throw new IllegalStateException("Seems "+nameForError+" wasn't warmed up, and so is "+
    "sizing up to accomodate \""+value+"\", with new contents: "+domain.categories)
    index
  }

  // Here's the Factorie objects
  val model = new StreamModel()
  val modelTrainingClone = new StreamModel()
  val weightsReadWriteLock : ReadWriteLock = new ReentrantReadWriteLock()

  class StreamModel extends Model with Parameters {

    // This can take both NodeType and FactorType, and will return a Weights(Tensor) that represents the weights
    // for this factor. These are going to be treated as **constant** during inference. They will be **variable** during
    // learning.

    // This should only ever get called from getDotFamilyWithStatisticsFor, otherwise it's a mess

    private def getWeightTensorFor(elemType : WithDomain, frozenDomain : Map[CategoricalDomain[String], CategoricalDomain[String]]) : Weights = {

      def getFrozenDomainOrIdentity(domain : CategoricalDomain[String]) : CategoricalDomain[String] = {
        if (frozenDomain != null && frozenDomain.contains(domain)) {
          frozenDomain(domain)
        }
        else {
          domain
        }
      }

      // Do any updates threadsafe
      elemType match {
        case factorType: FactorType =>
          // Size the tensor
          factorType.neighborTypes.size match {
            case 1 =>
              val neighbor1Domain = getFrozenDomainOrIdentity(factorType.neighborTypes(0).valueDomain)
              val featureDomain = getFrozenDomainOrIdentity(factorType.featureDomain)

              val numNode1Values = neighbor1Domain.length
              val numFeatures = featureDomain.length

              // Need to be careful to provide a fresh initializer inside of the Weights() call, b/c otherwise
              // weights will fail to clone correctly, and linesearch will flip a shit
              val w = Weights(new la.DenseTensor2(
                numNode1Values,
                numFeatures))

              val factorTensor = w.value.asInstanceOf[DenseTensor2]
              factorTensor.*=(0)

              // Populate the tensor
              if (factorType.weights != null) for (assignmentFeaturesPair <- factorType.weights) {
                val assignment = assignmentFeaturesPair._1
                val node1ValueIndex = getIndexCautious(neighbor1Domain, assignment(0), "neighborTypes(0).valueDomain")
                for (featureWeightPair <- assignmentFeaturesPair._2) {
                  val featureIndex = getIndexCautious(featureDomain, featureWeightPair._1, "factorType.featureDomain")
                  factorTensor.+=(node1ValueIndex, featureIndex, featureWeightPair._2)
                }
              }
              w
            case 2 =>
              val neighbor1Domain = getFrozenDomainOrIdentity(factorType.neighborTypes(0).valueDomain)
              val neighbor2Domain = getFrozenDomainOrIdentity(factorType.neighborTypes(1).valueDomain)
              val featureDomain = getFrozenDomainOrIdentity(factorType.featureDomain)

              val numNode1Values = neighbor1Domain.length
              val numNode2Values = neighbor2Domain.length
              val numFeatures = featureDomain.length

              // Need to be careful to provide a fresh initializer inside of the Weights() call, b/c otherwise
              // weights will fail to clone correctly, and linesearch will flip a shit
              val w = Weights(new la.DenseTensor3(
                numNode1Values,
                numNode2Values,
                numFeatures))

              val factorTensor = w.value
              factorTensor.*=(0)

              // Populate the tensor
              if (factorType.weights != null) for (assignmentFeaturesPair <- factorType.weights) {
                val assignment = assignmentFeaturesPair._1
                val node1ValueIndex = getIndexCautious(neighbor1Domain, assignment(0), "neighborTypes(0).valueDomain")
                val node2ValueIndex = getIndexCautious(neighbor2Domain, assignment(1), "neighborTypes(1).valueDomain")
                for (featureWeightPair <- assignmentFeaturesPair._2) {
                  val featureIndex = getIndexCautious(featureDomain, featureWeightPair._1, "factorType.featureDomain")
                  factorTensor.+=(node1ValueIndex, node2ValueIndex, featureIndex, featureWeightPair._2)
                }
              }
              w
            case 3 =>
              val neighbor1Domain = getFrozenDomainOrIdentity(factorType.neighborTypes(0).valueDomain)
              val neighbor2Domain = getFrozenDomainOrIdentity(factorType.neighborTypes(1).valueDomain)
              val neighbor3Domain = getFrozenDomainOrIdentity(factorType.neighborTypes(2).valueDomain)
              val featureDomain = getFrozenDomainOrIdentity(factorType.featureDomain)

              val numNode1Values = neighbor1Domain.length
              val numNode2Values = neighbor2Domain.length
              val numNode3Values = neighbor3Domain.length
              val numFeatures = featureDomain.length

              // Need to be careful to provide a fresh initializer inside of the Weights() call, b/c otherwise
              // weights will fail to clone correctly, and linesearch will flip a shit
              val w = Weights(new la.DenseTensor4(
                numNode1Values,
                numNode2Values,
                numNode3Values,
                numFeatures))

              val factorTensor = w.value
              factorTensor.*=(0)

              // Populate the tensor
              if (factorType.weights != null) for (assignmentFeaturesPair <- factorType.weights) {
                val assignment = assignmentFeaturesPair._1
                val node1ValueIndex = getIndexCautious(neighbor1Domain, assignment(0), "neighborTypes(0).valueDomain")
                val node2ValueIndex = getIndexCautious(neighbor2Domain, assignment(1), "neighborTypes(1).valueDomain")
                val node3ValueIndex = getIndexCautious(neighbor3Domain, assignment(2), "neighborTypes(2).valueDomain")
                for (featureWeightPair <- assignmentFeaturesPair._2) {
                  val featureIndex = getIndexCautious(featureDomain, featureWeightPair._1, "factorType.featureDomain")
                  factorTensor.+=(node1ValueIndex, node2ValueIndex, node3ValueIndex, featureIndex, featureWeightPair._2)
                }
              }
              w
            case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
          }

        case nodeType: NodeType =>
          // Size the tensor
          val nodeDomain = getFrozenDomainOrIdentity(nodeType.valueDomain)
          val featureDomain = getFrozenDomainOrIdentity(nodeType.featureDomain)

          val numNodeValues = nodeDomain.length
          val numFeatures = featureDomain.length

          // Need to be careful to provide a fresh initializer inside of the Weights() call, b/c otherwise
          // weights will fail to clone correctly, and linesearch will flip a shit
          val w = Weights(new la.DenseTensor2(numNodeValues,
            numFeatures))

          val nodeTensor = w.value
          nodeTensor.*=(0)

          // Populate the tensor
          if (nodeType.weights != null) for (valueFeaturesPair <- nodeType.weights) {
            val valueIndex = getIndexCautious(nodeDomain, valueFeaturesPair._1, "nodeType.valueDomain")
            for (featureWeightPair <- valueFeaturesPair._2) {
              val featureIndex = getIndexCautious(featureDomain, featureWeightPair._1, "nodeType.featureDomain")
              nodeTensor.+=(valueIndex, featureIndex, featureWeightPair._2)
            }
          }
          w
      }
    }

    val dotFamilyCache : java.util.IdentityHashMap[WithDomain, DotFamily] = new util.IdentityHashMap[WithDomain, DotFamily]()
    def getDotFamilyWithStatisticsFor(elemType : WithDomain, frozenDomain : Map[CategoricalDomain[String], CategoricalDomain[String]] = null) : DotFamily = {
      dotFamilyCache.synchronized {
        if (dotFamilyCache.containsKey(elemType)) {
          return dotFamilyCache.get(elemType)
        }
        if (!dotFamilyCache.containsKey(elemType)) {
          elemType match {
            case factorType: FactorType =>
              factorType.neighborTypes.size match {
                case 1 =>
                  dotFamilyCache.put(elemType, new DotFamilyWithStatistics2[CategoricalVariable[String],
                    FeatureVectorVariable[String]] {
                    val weights = getWeightTensorFor(elemType, frozenDomain).asInstanceOf[Weights2]
                    // Initialize the vector
                    limitedDiscreteValues1 = new SparseBinaryTensor1(factorType.neighborTypes(0).valueDomain.dimensionSize)
                    // Set all entries to "true"
                    for (i <- 0 to limitedDiscreteValues1.size - 1) limitedDiscreteValues1.+=(i, 1.0)
                  })
                case 2 =>
                  dotFamilyCache.put(elemType, new DotFamilyWithStatistics3[CategoricalVariable[String],
                    CategoricalVariable[String],
                    FeatureVectorVariable[String]] {
                    val weights = getWeightTensorFor(elemType, frozenDomain).asInstanceOf[Weights3]
                    // Initialize the vector
                    limitedDiscreteValues12 = new SparseBinaryTensor2(factorType.neighborTypes(0).valueDomain.dimensionSize, factorType.neighborTypes(1).valueDomain.dimensionSize)
                    // Set all entries to "true"
                    for (i <- 0 to limitedDiscreteValues12.size - 1) limitedDiscreteValues12.+=(i, 1.0)
                  })
                case 3 =>
                  dotFamilyCache.put(elemType, new DotFamilyWithStatistics4[CategoricalVariable[String],
                    CategoricalVariable[String],
                    CategoricalVariable[String],
                    FeatureVectorVariable[String]] {
                    val weights = getWeightTensorFor(elemType, frozenDomain).asInstanceOf[Weights4]
                  })
                case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
              }
            case nodeType: NodeType => dotFamilyCache.put(elemType, new DotFamilyWithStatistics2[CategoricalVariable[String], FeatureVectorVariable[String]] {
              val weights = getWeightTensorFor(elemType, frozenDomain).asInstanceOf[Weights2]
            })
            case otherType => throw new IllegalStateException("Got type neither NodeType or FactorType")
          }
        }

        val dotFamily = dotFamilyCache.get(elemType)
        if (dotFamily == null) throw new IllegalStateException("Shouldn't ever return a null dotFamily")
        dotFamily
      }
    }

    // This can take both Node and Factor objects, and return a variable representing the feature values for the
    // object. This will always be a **constant**, so should never be passed into the model as part of the list of factors.

    case class FeatureVariable(variable : GraphVarWithDomain, frozenDomain : Map[CategoricalDomain[String], CategoricalDomain[String]]) extends FeatureVectorVariable[String] with CaseClassEq {
      override lazy val domain: CategoricalVectorDomain[String] = {
        if (frozenDomain == null || !frozenDomain.contains(variable.domainType.domain.dimensionDomain)) {
          variable.domainType.domain
        }
        else {
          val newDimensionDomain = frozenDomain(variable.domainType.domain.dimensionDomain)
          new CategoricalVectorDomain[String] {
            override val dimensionDomain = newDimensionDomain
          }
        }
      }
    }

    def getFeatureVariableFor(variable : GraphVarWithDomain, frozenDomain : Map[CategoricalDomain[String], CategoricalDomain[String]]) : FeatureVariable = {
      val features = FeatureVariable(variable, frozenDomain)
      if (frozenDomain != null && frozenDomain.contains(features.domain.dimensionDomain)) {
        throw new IllegalStateException("Should have replaced the domain of the features variable")
      }

      variable match {
        case factor: GraphFactor =>
          factor.nodeList.size match {
            case 1 =>
              throw new NotImplementedError()
            case 2 =>
              if (factor.features != null)
                for (featureWeightPair <- factor.features) {
                  val featureIndex = getIndexCautious(features.domain.dimensionDomain, featureWeightPair._1, "factorType.featureDomain")
                  features += (featureWeightPair._1, featureWeightPair._2)
                }
            case 3 =>
              throw new NotImplementedError()
            case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
          }
        case node: GraphNode =>
          if (node.features != null)
            for (featureWeightPair <- node.features) {
              val featureIndex = getIndexCautious(features.domain.dimensionDomain, featureWeightPair._1, "domainType.featureDomain")
              features += (featureWeightPair._1, featureWeightPair._2)
            }
      }

      features
    }

    def warmUpIndexes(graph : Graph) : Unit = {
      val clearCacheIfSizeChanges = true
      // Pre-warm node type weights
      for (nodeType <- graph.nodes.map(_.nodeType).distinct) {
        if (nodeType.weights != null) for (valueFeaturesPair <- nodeType.weights) {
          val oldSize = nodeType.featureDomain.length
          nodeType.valueDomain.index(valueFeaturesPair._1)
          for (featureWeightPair <- valueFeaturesPair._2) {
            nodeType.featureDomain.index(featureWeightPair._1)
          }
          if (clearCacheIfSizeChanges && nodeType.featureDomain.length > oldSize) {
            dotFamilyCache.synchronized {
              dotFamilyCache.remove(nodeType)
            }
          }
        }
      }
      // Pre-warm factor type weights
      for (factorType <- graph.factors.map(_.factorType).distinct) {
        if (factorType.weights != null) for (valueFeaturesPair <- factorType.weights) {
          val oldSize = factorType.featureDomain.length
          for (featureWeightPair <- valueFeaturesPair._2) {
            factorType.featureDomain.index(featureWeightPair._1)
          }
          if (clearCacheIfSizeChanges && factorType.featureDomain.length > oldSize) {
            dotFamilyCache.synchronized {
              dotFamilyCache.remove(factorType)
            }
          }
        }
      }
      // Pre-warm node values, and node features
      for (node <- graph.nodes) {
        if (node.features != null) {
          for (featureWeightPair <- node.features) {
            val oldSize = node.nodeType.featureDomain.length
            node.nodeType.featureDomain.index(featureWeightPair._1)
            // Clear cached elements if we change the feature domain size
            if (clearCacheIfSizeChanges && node.nodeType.featureDomain.length > oldSize) {
              dotFamilyCache.synchronized {
                dotFamilyCache.remove(node.nodeType)
              }
            }
          }
        }
        if (node.observedValue != null) {
          node.nodeType.valueDomain.index(node.observedValue)
        }
      }
      for (factor <- graph.factors) {
        if (factor.features != null) {
          for (featureWeightPair <- factor.features) {
            val oldSize = factor.factorType.featureDomain.length
            factor.factorType.featureDomain.index(featureWeightPair._1)
            // Clear cached elements if we change the feature domain size
            if (clearCacheIfSizeChanges && factor.factorType.featureDomain.length > oldSize) {
              dotFamilyCache.synchronized {
                dotFamilyCache.remove(factor.factorType)
              }
            }
          }
        }
      }
    }

    def getNodeFactor(nodeVar : NodeVariable, frozenDomain : Map[CategoricalDomain[String], CategoricalDomain[String]]) : Factor = {
      val featureVariable = getFeatureVariableFor(nodeVar.node, frozenDomain)
      val family = getDotFamilyWithStatisticsFor(nodeVar.node.nodeType, frozenDomain)
        // Due to irritations with the type system and FACTORIE design with multiple classes instead of varargs, this
        // cruft is necessary here
        .asInstanceOf[DotFamilyWithStatistics2[CategoricalVariable[String], FeatureVectorVariable[String]]]
      family.Factor(nodeVar, featureVariable)
    }

    def getFactor(factor : GraphFactor, frozenDomain : Map[CategoricalDomain[String], CategoricalDomain[String]]) : Factor = {
        val featureVariable = getFeatureVariableFor(factor, frozenDomain)
        factor.nodeList.size match {
          case 1 =>
            val family = getDotFamilyWithStatisticsFor(factor.factorType, frozenDomain)
              // Due to irritations with the type system and FACTORIE design with multiple classes instead of varargs, this
              // cruft is necessary here
              .asInstanceOf[DotFamilyWithStatistics2[CategoricalVariable[String], FeatureVectorVariable[String]]]
            family.Factor(factor.nodeList(0).variable, featureVariable)
          case 2 =>
            val family = getDotFamilyWithStatisticsFor(factor.factorType, frozenDomain)
              // Due to irritations with the type system and FACTORIE design with multiple classes instead of varargs, this
              // cruft is necessary here
              .asInstanceOf[DotFamilyWithStatistics3[CategoricalVariable[String], CategoricalVariable[String], FeatureVectorVariable[String]]]
            family.Factor(factor.nodeList(0).variable, factor.nodeList(1).variable, featureVariable)
          case 3 =>
            val family = getDotFamilyWithStatisticsFor(factor.factorType, frozenDomain)
              // Due to irritations with the type system and FACTORIE design with multiple classes instead of varargs, this
              // cruft is necessary here
              .asInstanceOf[DotFamilyWithStatistics4[CategoricalVariable[String], CategoricalVariable[String], CategoricalVariable[String], FeatureVectorVariable[String]]]
            family.Factor(factor.nodeList(0).variable, factor.nodeList(1).variable, factor.nodeList(2).variable, featureVariable)
          case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
        }
    }

    override def factors(mutableVariables: Iterable[Var]): Iterable[Factor] = {
      if (mutableVariables.size == 0) return List()
      val mutableList : List[NodeVariable] = mutableVariables.toList.asInstanceOf[List[NodeVariable]]

      // Get the frozen domain, for parallel learning, if we're using it

      val frozenDomain = mutableList(0).frozenDomainMap

      // Warm up the variable domains

      val graph : Graph = mutableList(0).graph

      // Create the factors collection

      val result = newFactorsCollection

      // Add the unary factors

      mutableVariables.foreach{
        case nodeVar : NodeVariable =>
          val f = getNodeFactor(nodeVar, frozenDomain)
          result += f
          nodeVar.set(0)(null)
      }

      def isMutableNode(n : GraphNode) : Boolean = {
        for (m <- mutableList) {
          if (m.node eq n) return true
        }
        false
      }

      // Add the multi-node factors

      graph.factors.foreach(factor => {
        if (factor.nodeList.exists(isMutableNode)) {
          result += getFactor(factor, frozenDomain)
        }
      })

      result
    }
  }
}

