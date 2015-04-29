package edu.stanford.lense_base.graph

import cc.factorie.infer._
import cc.factorie.la
import cc.factorie.la._
import cc.factorie.model._
import cc.factorie.optimize._
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
}

case class GraphNode(graph : Graph,
                      nodeType: NodeType,
                      var features: Map[String, Double] = null,
                      var observedValue: String = null,
                      payload: Any = null) extends GraphVarWithDomain(nodeType) with CaseClassEq {

  val id: Int = graph.nodes.size
  graph.nodes += this

  val variable = NodeVariable(this, graph)

  if (features == null) features = Map("BIAS" -> 1.0)
  else if (!features.contains("BIAS")) features = features ++ Map("BIAS" -> 1.0)

  override def hashCode() : Int = nodeType.hashCode() + id.hashCode()
}

case class GraphFactor(graph : Graph,
                        factorType: FactorType,
                  nodes: Iterable[GraphNode],
                  var features: Map[String, Double] = null,
                  payload: Any = null) extends GraphVarWithDomain(factorType) with CaseClassEq {

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
}

case class Graph(stream : GraphStream) {
  val nodes: mutable.MutableList[GraphNode] = new mutable.MutableList[GraphNode]()
  val factors: mutable.MutableList[GraphFactor] = new mutable.MutableList[GraphFactor]()

  override def clone() : (Graph, Map[GraphNode, GraphNode]) = {
    val g = stream.newGraph()

    // Double check for corruption
    for (factor <- factors) {
      for (node <- factor.nodes) {
        if (!nodes.contains(node)) throw new IllegalStateException("Corrupt state, contain nodes we're not supposed to")
      }
    }

    val oldToNew : Map[GraphNode, GraphNode] = nodes.map{node => {
      (node, g.makeNode(node.nodeType, node.features, node.observedValue, node.payload))
    }}.toMap
    for (factor <- factors) {
      g.makeFactor(factor.factorType, factor.nodes.map(oldToNew(_)), factor.features)
    }
    (g, oldToNew)
  }

  def makeNode(nodeType : NodeType, features : Map[String,Double] = null, observedValue: String = null, payload : Any = null) : GraphNode = {
    GraphNode(this, nodeType, features, observedValue, payload)
  }

  def makeFactor(factorType : FactorType, nodes : Iterable[GraphNode], features : Map[String,Double] = null, payload : Any = null) : GraphFactor = {
    for (node <- nodes) {
      if (!this.nodes.contains(node)) throw new IllegalStateException()
    }
    GraphFactor(this, factorType, nodes, features, payload)
  }

  def mapEstimate(): Map[GraphNode, String] = {
    val variables = allVariablesForFactorie()
    if (variables.size == 0) return Map()
    variables.foreach(_.includeHardConstantFactors = true)

    // Do exact inference via trees if possible.
    val sumMax = try {
      MaximizeByBPTree.infer(variables, stream.model)
    }
    // If that fails, perform loopy maximization
    catch {
      case _ : Throwable =>
        // run loopy bp
        MaximizeByBPLoopyTreewise.infer(variables, stream.model)
    }

    variables.filter(_.node.observedValue == null).map{
      case nodeVar : NodeVariable =>
        nodeVar.node -> (sumMax.getMarginal(nodeVar).get match {
          case dm : DiscreteMarginal1[NodeVariable] => dm.value1.category
          case sm : MAPSummary#SingletonMarginal =>
            sm.setToMaximize(null)
            nodeVar.categoryValue
        })
    }.toMap
  }

  def marginalEstimate(): Map[GraphNode, Map[String,Double]] = {
    val variables = allVariablesForFactorie()
    if (variables.size == 0) return Map()
    variables.foreach(_.includeHardConstantFactors = true)

    // Do exact inference via trees if possible.
    val sumMarginal = try {
      InferByBPTree.infer(variables, stream.model)
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
        InferByBPLoopyTreewise.infer(variables, stream.model)
    }

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

  def allVariablesForFactorie(): Seq[NodeVariable] = {
    nodes.map(_.variable)
  }
}

abstract class WithDomain(stream : GraphStream) {
  val featureDomain = new CategoricalDomain[String]
  val domain = new CategoricalVectorDomain[String] { override def dimensionDomain = featureDomain }
  stream.withDomainList += this
}

case class NodeType(stream : GraphStream, possibleValues : Set[String], var weights : Map[String,Map[String,Double]] = null) extends WithDomain(stream) with CaseClassEq {
  val valueDomain = new CategoricalDomain[String]
  valueDomain.indexAll(possibleValues)

  override def hashCode : Int = possibleValues.hashCode()

  def setWeights(newWeights : Map[String,Map[String,Double]]) = {
    weights = newWeights
    stream.model.weightsTensorCache.remove(this)
    stream.model.dotFamilyCache.remove(this)
  }
}

case class FactorType(stream : GraphStream, neighborTypes : List[NodeType], var weights : Map[List[String],Map[String,Double]] = null) extends WithDomain(stream) with CaseClassEq {
  if (neighborTypes.size < 1 || neighborTypes.size > 3)
    throw new UnsupportedOperationException("FactorType doesn't support neighbor lists smaller than 1, or larger "+
      "than 3, due to underlying design decisions in FACTORIE making larger factor support a total pain in the ass.")

  override def hashCode : Int = neighborTypes.hashCode()

  def setWeights(newWeights : Map[List[String],Map[String,Double]]) = {
    weights = newWeights
    stream.model.weightsTensorCache.remove(this)
    stream.model.dotFamilyCache.remove(this)
  }
}

// This holds the Multi-class decision variable component of a given node.
// The model expects a list of these to be passed in.

case class NodeVariable(node : GraphNode, graph : Graph) extends CategoricalVariable[String] {

  // TODO: Keeping this state here will lead to race conditions, unfortunately, but it seems there is no way to pass
  // the variable through
  var includeHardConstantFactors = false

  override def domain = node.nodeType.valueDomain
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

  def newGraph() : Graph = Graph(this)

  // learns the appropriate bits, which means any weight factors, and EM if there are any
  // nodes with unobserved values. This is called for its byproducts, and will just go in and update the existing
  // weights on the NodeTypes and FactorTypes that are involved in the graphs that were passed in.

  def learn(graphs : Iterable[Graph], regularization: Double = 1.0) = {
    if (graphs.exists(graph => graph.nodes.exists(node => {
      node.observedValue == null
    }))) learnEM(graphs)
    else learnFullyObserved(graphs, regularization)

    // Now we need to decode the weights

    for (withDomain <- withDomainList) {
      if (!model.dotFamilyCache.contains(withDomain)) {
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
              if (tensor.dimensions(0) != nodeType.possibleValues.size) throw new IllegalStateException()
              if (tensor.dimensions(1) != nodeType.featureDomain.size) throw new IllegalStateException()
              // we need the weight values for each possible assignment
              val tensor2 = tensor.asInstanceOf[Tensor2]
              val keyValue: ListBuffer[(String, Map[String, Double])] = ListBuffer()
              nodeType.possibleValues.foreach(value => {
                val valueIndex = nodeType.valueDomain.index(value)
                for (val1 <- nodeType.possibleValues) {
                  val index1 = nodeType.valueDomain.index(val1)
                  keyValue += val1 ->
                    nodeType.featureDomain.categories.map(feature => {
                      val featIndex = nodeType.featureDomain.index(feature)
                      val weight = tensor2(index1, featIndex)
                      feature -> weight
                    }).toMap
                }
              })
              nodeType.weights = keyValue.toMap

            // Translate a factorType's weights back into the Map we use for weights

            case factorType: FactorType =>
              factorType.neighborTypes.size match {
                case 1 =>
                  if (tensor.dimensions.length != 3) throw new IllegalStateException("Can't have weights for a Factor's " +
                    "factor that isn't val1 x features, which means dim=2. Instead got dim=" + tensor.dimensions.length)
                  if (tensor.dimensions(0) != factorType.neighborTypes(0).possibleValues.size) throw new IllegalStateException()
                  if (tensor.dimensions(1) != factorType.featureDomain.size) throw new IllegalStateException()

                  val tensor2 = tensor.asInstanceOf[Tensor2]
                  val keyValue: ListBuffer[(List[String], Map[String, Double])] = ListBuffer()
                  for (val1 <- factorType.neighborTypes(0).possibleValues) {
                    val index1 = factorType.neighborTypes(0).valueDomain.index(val1)
                    keyValue += List(val1) ->
                      factorType.featureDomain.categories.map(feature => {
                        val featIndex = factorType.featureDomain.index(feature)

                        val weight = tensor2(index1, featIndex)
                        feature -> weight
                      }).toMap
                  }
                  factorType.weights = keyValue.toMap
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
                        factorType.featureDomain.categories.map(feature => {
                          val featIndex = factorType.featureDomain.index(feature)

                          val weight = tensor3(index1, index2, featIndex)
                          feature -> weight
                        }).toMap
                    }
                  }
                  factorType.weights = keyValue.toMap
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
                          factorType.featureDomain.categories.map(feature => {
                            val featIndex = factorType.featureDomain.index(feature)

                            val weight = tensor4(index1, index2, index3, featIndex)
                            feature -> weight
                          }).toMap
                      }
                    }
                  }
                  factorType.weights = keyValue.toMap
                case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
              }
          }
        }
      }
    }
  }

  // performs EM on the graph. Still massively TODO

  private def learnEM(graphs : Iterable[Graph]) = {
    throw new UnsupportedOperationException("We don't yet support EM. Make sure all your variables have observed values.")
  }

  // This will learn just Weight() values from the fully observed values in the graphs

  private def learnFullyObserved(graphs : Iterable[Graph], regularization : Double): Unit = {

    // Need to clear all the weights beforehand, because for some reason if we don't then
    // they simultaneously update and effect the updates (bc they change E[x])

    for (w <- model.weightsTensorCache) {
      w._2.value.*=(0.0)
    }

    // println(model.parameters.toSeq)

    graphs.foreach(graph => graph.nodes.foreach(node => {
      node.variable.includeHardConstantFactors = false
      if (node.observedValue != null) {
        node.variable.set(node.nodeType.valueDomain.index(node.observedValue))(null)
      }
    }))

    val trainer = new BatchTrainer(model.parameters, new LBFGS() with L2Regularization{variance = regularization}, maxIterations = 10)
    val likelihoodExamples = graphs.map(graph => new LikelihoodExample(graph.allVariablesForFactorie(), model, InferByBPChain))
    trainer.trainFromExamples(likelihoodExamples)
  }


  def getIndexCautious(domain : CategoricalDomain[String], value : String, nameForError : String) : Int = {
    val oldSize = domain.size
    val index = domain.index(value)
    if (oldSize != domain.size) throw new IllegalStateException("Seems "+nameForError+" wasn't warmed up, and so is "+
    "sizing up to accomodate \""+value+"\", with new contents: "+domain.categories)
    index
  }

  // Here's the Factorie objects

  val model = new Model with Parameters {

    // This can take both NodeType and FactorType, and will return a Weights(Tensor) that represents the weights
    // for this factor. These are going to be treated as **constant** during inference. They will be **variable** during
    // learning.

    val weightsTensorCache : mutable.Map[WithDomain, Weights] = mutable.Map()
    def getWeightTensorFor(elemType : WithDomain) : Weights = {
      if (!weightsTensorCache.contains(elemType)) {
        elemType match {
          case factorType : FactorType =>
            // Size the tensor
            val tensor = factorType.neighborTypes.size match {
              case 1 =>
                val numNode1Values = factorType.neighborTypes(0).valueDomain.length
                val numFeatures = factorType.featureDomain.length
                val factorTensor = new la.DenseTensor2(numNode1Values, numFeatures)

                // Populate the tensor
                if (factorType.weights != null) for (assignmentFeaturesPair <- factorType.weights) {
                  val assignment = assignmentFeaturesPair._1
                  val node1ValueIndex = getIndexCautious(factorType.neighborTypes(0).valueDomain, assignment(0), "neighborTypes(0).valueDomain")
                  for (featureWeightPair <- assignmentFeaturesPair._2) {
                    val featureIndex = getIndexCautious(factorType.featureDomain, featureWeightPair._1, "factorType.featureDomain")
                    factorTensor.+=(node1ValueIndex, featureIndex, featureWeightPair._2)
                  }
                }
                weightsTensorCache.put(elemType, Weights(factorTensor))
              case 2 =>
                val numNode1Values = factorType.neighborTypes(0).valueDomain.length
                val numNode2Values = factorType.neighborTypes(1).valueDomain.length
                val numFeatures = factorType.featureDomain.length
                val factorTensor = new la.DenseTensor3(numNode1Values, numNode2Values, numFeatures)

                // Populate the tensor
                if (factorType.weights != null) for (assignmentFeaturesPair <- factorType.weights) {
                  val assignment = assignmentFeaturesPair._1
                  val node1ValueIndex = getIndexCautious(factorType.neighborTypes(0).valueDomain, assignment(0), "neighborTypes(0).valueDomain")
                  val node2ValueIndex = getIndexCautious(factorType.neighborTypes(1).valueDomain, assignment(1), "neighborTypes(1).valueDomain")
                  for (featureWeightPair <- assignmentFeaturesPair._2) {
                    val featureIndex = getIndexCautious(factorType.featureDomain, featureWeightPair._1, "factorType.featureDomain")
                    factorTensor.+=(node1ValueIndex, node2ValueIndex, featureIndex, featureWeightPair._2)
                  }
                }
                weightsTensorCache.put(elemType, Weights(factorTensor))
              case 3 =>
                val numNode1Values = factorType.neighborTypes(0).valueDomain.length
                val numNode2Values = factorType.neighborTypes(1).valueDomain.length
                val numNode3Values = factorType.neighborTypes(2).valueDomain.length
                val numFeatures = factorType.featureDomain.length
                val factorTensor = new la.DenseTensor4(numNode1Values, numNode2Values, numNode3Values, numFeatures)

                // Populate the tensor
                if (factorType.weights != null) for (assignmentFeaturesPair <- factorType.weights) {
                  val assignment = assignmentFeaturesPair._1
                  val node1ValueIndex = getIndexCautious(factorType.neighborTypes(0).valueDomain, assignment(0), "neighborTypes(0).valueDomain")
                  val node2ValueIndex = getIndexCautious(factorType.neighborTypes(1).valueDomain, assignment(1), "neighborTypes(1).valueDomain")
                  val node3ValueIndex = getIndexCautious(factorType.neighborTypes(2).valueDomain, assignment(2), "neighborTypes(2).valueDomain")
                  for (featureWeightPair <- assignmentFeaturesPair._2) {
                    val featureIndex = getIndexCautious(factorType.featureDomain, featureWeightPair._1, "factorType.featureDomain")
                    factorTensor.+=(node1ValueIndex, node2ValueIndex, node3ValueIndex, featureIndex, featureWeightPair._2)
                  }
                }
                weightsTensorCache.put(elemType, Weights(factorTensor))
              case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
            }

          case nodeType : NodeType =>
            // Size the tensor
            val numNodeValues = nodeType.valueDomain.length
            val numFeatures = nodeType.featureDomain.length
            val nodeTensor = new la.DenseTensor2(numNodeValues, numFeatures)

            // Populate the tensor
            if (nodeType.weights != null) for (valueFeaturesPair <- nodeType.weights) {
              val valueIndex = getIndexCautious(nodeType.valueDomain, valueFeaturesPair._1, "nodeType.valueDomain")
              for (featureWeightPair <- valueFeaturesPair._2) {
                val featureIndex = getIndexCautious(nodeType.featureDomain, featureWeightPair._1, "nodeType.featureDomain")
                nodeTensor.+=(valueIndex, featureIndex, featureWeightPair._2)
              }
            }

            // Cache the tensor as a Weight
            weightsTensorCache.put(elemType, Weights(nodeTensor))
        }
      }
      weightsTensorCache.get(elemType).get
    }

    val dotFamilyCache : mutable.Map[WithDomain, DotFamily] = mutable.Map()
    def getDotFamilyWithStatisticsFor(elemType : WithDomain) : DotFamily = {
      if (!dotFamilyCache.contains(elemType)) {
        elemType match {
          case factorType: FactorType =>
            factorType.neighborTypes.size match {
              // TODO: Test this
              case 1 =>
                dotFamilyCache.put(elemType, new DotFamilyWithStatistics2[CategoricalVariable[String],
                  FeatureVectorVariable[String]] {
                  val weights = getWeightTensorFor(elemType).asInstanceOf[Weights2]
                  // Initialize the vector
                  limitedDiscreteValues1 = new SparseBinaryTensor1(factorType.neighborTypes(0).valueDomain.dimensionSize)
                  // Set all entries to "true"
                  for (i <- 0 to limitedDiscreteValues1.size-1) limitedDiscreteValues1.+=(i, 1.0)
                })
              case 2 =>
                dotFamilyCache.put(elemType, new DotFamilyWithStatistics3[CategoricalVariable[String],
                  CategoricalVariable[String],
                  FeatureVectorVariable[String]] {
                  val weights = getWeightTensorFor(elemType).asInstanceOf[Weights3]
                  // Initialize the vector
                  limitedDiscreteValues12 = new SparseBinaryTensor2(factorType.neighborTypes(0).valueDomain.dimensionSize, factorType.neighborTypes(1).valueDomain.dimensionSize)
                  // Set all entries to "true"
                  for (i <- 0 to limitedDiscreteValues12.size-1) limitedDiscreteValues12.+=(i, 1.0)
                })
              case 3 =>
                dotFamilyCache.put(elemType, new DotFamilyWithStatistics4[CategoricalVariable[String],
                  CategoricalVariable[String],
                  CategoricalVariable[String],
                  FeatureVectorVariable[String]] {
                  val weights = getWeightTensorFor(elemType).asInstanceOf[Weights4]
                })
              case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
            }
          case nodeType: NodeType => dotFamilyCache.put(elemType, new DotFamilyWithStatistics2[CategoricalVariable[String], FeatureVectorVariable[String]] {
            val weights = getWeightTensorFor(elemType).asInstanceOf[Weights2]
          })
        }
      }
      dotFamilyCache.get(elemType).get
    }

    // This can take both Node and Factor objects, and return a variable representing the feature values for the
    // object. This will always be a **constant**, so should never be passed into the model as part of the list of factors.

    val featureVariableCache = mutable.Map[GraphVarWithDomain, FeatureVariable]()

    case class FeatureVariable(variable : GraphVarWithDomain) extends FeatureVectorVariable[String] {
      override def domain: CategoricalVectorDomain[String] = variable.domainType.domain
    }
    def getFeatureVariableFor(variable : GraphVarWithDomain) : FeatureVariable = {
      if (!featureVariableCache.contains(variable)) {
        val features = FeatureVariable(variable)

        variable match {
          case factor: GraphFactor =>
            factor.nodeList.size match {
              case 1 =>
              case 2 =>
                if (factor.features != null) for (featureWeightPair <- factor.features) {
                  val featureIndex = getIndexCautious(factor.factorType.featureDomain, featureWeightPair._1, "factorType.featureDomain")
                  features.update(featureIndex, featureWeightPair._2)(null)
                }
              case 3 =>
              case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
            }
          case node: GraphNode =>
            if (node.features != null) for (featureWeightPair <- node.features) {
              val featureIndex = getIndexCautious(variable.domainType.featureDomain, featureWeightPair._1, "domainType.featureDomain")
              features.update(featureIndex, featureWeightPair._2)(null)
            }
        }

        featureVariableCache.put(variable, features)
      }
      featureVariableCache(variable)
    }

    def warmUpIndexes(graph : Graph) = {
      // Pre-warm node type weights
      for (nodeType <- graph.nodes.map(_.nodeType).distinct) {
        if (nodeType.weights != null) for (valueFeaturesPair <- nodeType.weights) {
          nodeType.valueDomain.index(valueFeaturesPair._1)
          for (featureWeightPair <- valueFeaturesPair._2) {
            nodeType.featureDomain.index(featureWeightPair._1)
          }
        }
      }
      // Pre-warm factor type weights
      for (factorType <- graph.factors.map(_.factorType).distinct) {
        if (factorType.weights != null) for (valueFeaturesPair <- factorType.weights) {
          for (featureWeightPair <- valueFeaturesPair._2) {
            factorType.featureDomain.index(featureWeightPair._1)
          }
        }
      }
      for (node <- graph.nodes) {
        if (node.features != null) for (featureWeightPair <- node.features) {
          val oldSize = node.nodeType.featureDomain.length
          node.nodeType.featureDomain.index(featureWeightPair._1)
          // Clear cached elements if we change the feature domain size
          if (node.nodeType.featureDomain.length > oldSize) {
            dotFamilyCache.remove(node.nodeType)
            weightsTensorCache.remove(node.nodeType)
          }
        }
      }
      for (factor <- graph.factors) {
        if (factor.features != null) for (featureWeightPair <- factor.features) {
          val oldSize = factor.factorType.featureDomain.length
          factor.factorType.featureDomain.index(featureWeightPair._1)
          // Clear cached elements if we change the feature domain size
          if (factor.factorType.featureDomain.length > oldSize) {
            dotFamilyCache.remove(factor.factorType)
            weightsTensorCache.remove(factor.factorType)
          }
        }
      }
    }

    val nodeFactorCache : mutable.Map[NodeVariable, Factor] = mutable.Map()
    def getNodeFactor(nodeVar : NodeVariable) : Factor = {
      if (nodeVar.node.observedValue != null && nodeVar.includeHardConstantFactors) {
        nodeFactorCache.put(nodeVar, getConstantFactor(nodeVar.node))
      }
      else {
        val featureVariable = getFeatureVariableFor(nodeVar.node)
        val family = getDotFamilyWithStatisticsFor(nodeVar.node.nodeType)
          // Due to irritations with the type system and FACTORIE design with multiple classes instead of varargs, this
          // cruft is necessary here
          .asInstanceOf[DotFamilyWithStatistics2[CategoricalVariable[String], FeatureVectorVariable[String]]]
        nodeFactorCache.put(nodeVar, family.Factor(nodeVar, featureVariable))
      }
      nodeFactorCache(nodeVar)
    }

    def getConstantFactor(node : GraphNode) : Factor1[NodeVariable] = {
      val idx = node.nodeType.valueDomain.index(node.observedValue)
      val hardWeights = new DenseTensor1(node.nodeType.valueDomain.dimensionSize)
      for (i <- 0 to hardWeights.size-1) if (i != idx) hardWeights.+=(i, Double.NegativeInfinity)

      new DotFactorWithStatistics1[NodeVariable](node.variable) {
        override val weights: Tensor = hardWeights
      }
    }

    val factorCache : mutable.Map[GraphFactor, Factor] = mutable.Map()
    def getFactor(factor : GraphFactor) : Factor = {
      //if (!factorCache.contains(factor)) {
        val featureVariable = getFeatureVariableFor(factor)
        factorCache.put(factor, factor.nodeList.size match {
          case 1 =>
            val family = getDotFamilyWithStatisticsFor(factor.factorType)
              // Due to irritations with the type system and FACTORIE design with multiple classes instead of varargs, this
              // cruft is necessary here
              .asInstanceOf[DotFamilyWithStatistics2[CategoricalVariable[String], FeatureVectorVariable[String]]]
            family.Factor(factor.nodeList(0).variable, featureVariable)
          case 2 =>
            val family = getDotFamilyWithStatisticsFor(factor.factorType)
              // Due to irritations with the type system and FACTORIE design with multiple classes instead of varargs, this
              // cruft is necessary here
              .asInstanceOf[DotFamilyWithStatistics3[CategoricalVariable[String], CategoricalVariable[String], FeatureVectorVariable[String]]]
            family.Factor(factor.nodeList(0).variable, factor.nodeList(1).variable, featureVariable)
          case 3 =>
            val family = getDotFamilyWithStatisticsFor(factor.factorType)
              // Due to irritations with the type system and FACTORIE design with multiple classes instead of varargs, this
              // cruft is necessary here
              .asInstanceOf[DotFamilyWithStatistics4[CategoricalVariable[String], CategoricalVariable[String], CategoricalVariable[String], FeatureVectorVariable[String]]]
            family.Factor(factor.nodeList(0).variable, factor.nodeList(1).variable, factor.nodeList(2).variable, featureVariable)
          case _ => throw new IllegalStateException("FactorType shouldn't have a neighborTypes that's size is <1 or >3")
        })
      //}
      factorCache(factor)
    }

    override def factors(mutableVariables: Iterable[Var]): Iterable[Factor] = {
      if (mutableVariables.size == 0) return List()
      val mutableList : List[NodeVariable] = mutableVariables.toList.asInstanceOf[List[NodeVariable]]

      // Warm up the variable domains

      val graph : Graph = mutableList(0).graph
      warmUpIndexes(graph)

      // Create the factors collection

      val result = newFactorsCollection

      // Add the unary factors

      mutableVariables.foreach{
        case nodeVar : NodeVariable =>
          result += getNodeFactor(nodeVar)
      }

      def isMutableNode(n : GraphNode) : Boolean = {
        mutableList.contains(n.variable)
      }

      // Add the multi-node factors

      graph.factors.foreach(factor => {
        if (factor.nodeList.exists(isMutableNode)) {
          result += getFactor(factor)
        }
      })

      result
    }
  }
}

