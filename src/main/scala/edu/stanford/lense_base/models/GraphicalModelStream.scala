package edu.stanford.lense_base.models

import java.util

import edu.stanford.lense_base.graph._
import edu.stanford.lense_base.humancompute.{HumanComputeUnit, HumanErrorDistribution}

import scala.collection.mutable

import scala.collection.JavaConversions._

/**
 * Created by keenon on 5/28/15.
 *
 * Implements one version of a ModelStream that provides graphical models, and can handle inference and observations over
 * these graphical models. Implemented on top of GraphStream, which in turn is on top of Factorie
 */
class GraphicalModelStream(humanErrorDistribution : HumanErrorDistribution) extends ModelStream(humanErrorDistribution) {
  val graphStream : GraphStream = new GraphStream()

  /**
   * Retrains the model based on all examples seen so far.
   * @return model loss, for tracking and debugging optimizers
   */
  override def learn(models : Iterable[Model]): Double = {
    // Set all the graphs to the MAP
    models.foreach(m => {
      val gm = m.asInstanceOf[GraphicalModel]
      gm.setVariablesToMAP()

      /*
      // Double check by printing
      if (gm.variables.exists(v => !v.isObserved)) {
        gm.variables.foreach(v => {
          println("Value: "+v.asInstanceOf[GraphicalModelVariable].node.observedValue)
          if (v.asInstanceOf[GraphicalModelVariable].node.observedValue == null) {
            throw new IllegalStateException("Can't have a null observed value, we just set it!")
          }
        })
      }
      */
    })

    val mapLoss = 0.0

    // Run learning - MAP estimates for initialization
    println("RUNNING MAP INITIALIZATION...")
    val mapLoss = graphStream.learn(models.map(_.asInstanceOf[GraphicalModel].getGraph))

    // Reset human weights to default, because regularizer will have messed with them, even though likelihoods should not have changed
    for (humanObservationTypePair <- humanObservationTypesCache.values) {
      humanObservationTypePair._2.setWeights(getInitialHumanErrorGuessWeights(humanObservationTypePair._1.possibleValues).asInstanceOf[Map[Any, Map[String,Double]]])
    }

    val loss = if (models.exists(m => m.variables.exists(!_.isObserved))) {
      // Run learning - soft EM
      println("RUNNING EM FINE TUNING...")
      val emLoss = graphStream.learn(models.map(_.asInstanceOf[GraphicalModel].getGraph))
      // Read out human weights
      for (humanObservationTypePair <- humanObservationTypesCache.values) {
        val w = humanObservationTypePair._2.getExpNormalizedWeights.asInstanceOf[Map[List[String],Double]]
        humanErrorDistribution.setLatestEMObservation(w)
        println("Human weights estimate: "+w)
      }
      emLoss
    } else mapLoss

    // Return loss
    loss
  }

  /**
   * Gets a new model (an empty graph, or a single logit with no features) from this modelStream, which can then be
   * manipulated and specialized according to the subclass type.
   * @return
   */
  override def newModel(): GraphicalModel = new GraphicalModel(this)

  /**
   * Gets a distribution over errors as a map that can be used for GraphStream
   * @param classes the possible values over which humans can guess
   * @return a map of feature weights for all possible joint assignments of true value and human guess
   */
  private def getInitialHumanErrorGuessWeights(classes : Set[String]) : Map[List[String], Map[String, Double]] = {
    classes.flatMap(cl1 => {
      classes.map(cl2 => {
        (List(cl1, cl2),
          Map[String,Double]("BIAS" -> Math.log(humanErrorDistribution.jointProbability(cl1, cl2))))
      })
    }).toMap
  }

  val humanObservationTypesCache = mutable.HashMap[NodeType, (NodeType, FactorType)]()

  /**
   * Gets the NodeType and FactorType for attaching human observations to graphs
   *
   * @param nodeType the nodeType to which we want to attach an observation
   * @return
   */
  def getHumanObservationTypes(nodeType : NodeType) : (NodeType, FactorType) = {
    if (!humanObservationTypesCache.contains(nodeType)) {
      val humanObservationNodeType = graphStream.makeNodeType(nodeType.possibleValues)
      val humanObservationFactorType = graphStream.makeFactorType(List(nodeType, humanObservationNodeType), getInitialHumanErrorGuessWeights(nodeType.possibleValues))
      humanObservationTypesCache.put(nodeType, (humanObservationNodeType, humanObservationFactorType))
    }
    humanObservationTypesCache(nodeType)
  }
}

class GraphicalModel(modelStream : GraphicalModelStream) extends Model(modelStream) {
  private var graph : Graph = null
  private var vars : List[GraphicalModelVariable] = null

  private var varToNode : java.util.IdentityHashMap[ModelVariable, GraphNode] = null
  private var nodeToVar : java.util.IdentityHashMap[GraphNode, GraphicalModelVariable] = null

  /**
   * You have to set this before your GraphicalModel is useful
   *
   * @param g graph to use
   */
  def setGraph(g : Graph) = {
    graph = g
    val pairs = graph.nodes.map(n => (GraphicalModelVariable(n, this), n))
    varToNode = new util.IdentityHashMap()
    nodeToVar = new util.IdentityHashMap()
    for (pair <- pairs) {
      varToNode.put(pair._1, pair._2)
      nodeToVar.put(pair._2, pair._1)
    }
    vars = pairs.map(_._1).toList
  }
  def getGraph = graph

  /**
   * This is the private version of setGraph, so that we can clone ourselves and still use the same variables as index
   *
   * @param g graph
   * @param overrideVarToNode a mapping describing how variables should correspond with nodes
   */
  private def setGraphWithVarToNode(g : Graph, overrideVarToNode : java.util.IdentityHashMap[ModelVariable, GraphNode]) = {
    graph = g
    varToNode = overrideVarToNode
    nodeToVar = new util.IdentityHashMap()
    for (key <- overrideVarToNode.keySet().iterator()) {
      nodeToVar.put(overrideVarToNode.get(key), key.asInstanceOf[GraphicalModelVariable])
    }
    vars = overrideVarToNode.keySet().toArray.map(_.asInstanceOf[GraphicalModelVariable]).toList
  }

  /**
   * Adds a new observation to the given variable, with updates according to the modelStream.humanErrorDistribution,
   * and returns the new model representing this new state of the world.
   *
   * @param variable the variable to which we will add an observation
   * @param observation the value observed
   * @return a new model, representing adding this observation
   */
  override def cloneModelWithHumanObservation(variable: ModelVariable, observation: String): Model = {
    val humanNodeAndFactorTypes = modelStream.getHumanObservationTypes(varToNode.get(variable).nodeType)

    val graphClonePair = graph.clone()
    val newGraph = graphClonePair._1
    val newVarToNode = new java.util.IdentityHashMap[ModelVariable, GraphNode]()
    for (v <- varToNode.keySet().iterator()) {
      newVarToNode.put(v, graphClonePair._2(varToNode.get(v)))
    }

    val humanObservationNode = newGraph.makeNode(humanNodeAndFactorTypes._1, observedValue = observation)
    newVarToNode.get(variable).numHumanObservations += 1

    newGraph.makeFactor(humanNodeAndFactorTypes._2, List(newVarToNode.get(variable), humanObservationNode))

    val modelClone = modelStream.newModel()

    // Set the clone up with our same variable pointers
    modelClone.setGraphWithVarToNode(newGraph, newVarToNode)

    modelClone
  }

  /**
   * @return a map of the variables -> distributions over tokens
   */
  override def marginals: Map[ModelVariable, Map[String, Double]] = {
    graph.marginalEstimate().map(p => (nodeToVar.get(p._1), p._2)) ++
      vars.filter(_.isObserved).map(v => (v.asInstanceOf[ModelVariable], v.possibleValues.map(q => (q, if (q == v.getObservedValue) 1.0 else 0.0)).toMap)).toMap
  }

  /**
   * @return a map of variables -> MAP assignment
   */
  override def map: Map[ModelVariable, String] = {
    graph.mapEstimate().map(p => (nodeToVar.get(p._1), p._2)) ++
      vars.filter(_.isObserved).map(v => (v.asInstanceOf[ModelVariable], v.getObservedValue)).toMap
  }

  /**
   * @return a list of the variables that this model covers
   */
  override def variables: List[ModelVariable] = vars

  def setVariablesToMAP() : Unit = {
    lazy val m = map
    for (variable <- vars) {
      if (variable.isObserved)
        varToNode.get(variable).observedValue = variable.getObservedValue
      else {
        varToNode.get(variable).observedValue = m(variable)
      }
    }
  }

  def setVariablesToNull() : Unit = {
    for (variable <- vars) {
      if (variable.isObserved)
        varToNode.get(variable).observedValue = variable.getObservedValue
      else {
        varToNode.get(variable).observedValue = null
      }
    }
  }
}

case class GraphicalModelVariable(node : GraphNode, gm : GraphicalModel) extends ModelVariable(gm) {
  override def possibleValues: List[String] = node.nodeType.possibleValues.toList
  def payload: Any = node.payload
}
