package edu.stanford.lense_base

import edu.stanford.lense_base.graph._

import scala.collection.mutable

/**
 * Created by keenon on 4/27/15.
 *
 * This is the central static dispatcher to handle requests to the API
 */
class Lense(stream : GraphStream) {
  val defaultEpsilon = 0.3

  def predict(graph : Graph, askHuman : GraphNode => String, lossFunction : (List[(GraphNode, String, Double)], Double, Double) => Double) : Map[GraphNode, String] = {
    // Stupid one-vote no-learning baseline
    graph.nodes.map(n => {
      (n, askHuman(n))
    }).toMap
  }

  // create initial BIAS weights guess, as a log scale normalized factor, using defaultEpsilon for error probability
  def getInitialHumanErrorGuessWeights(classes : Set[String]) : Map[List[String], Map[String, Double]] = {
    classes.flatMap(cl1 => {
      classes.map(cl2 => {
        (List(cl1, cl2),
          Map[String,Double]("BIAS" -> (if (cl1 == cl2) Math.log((1-defaultEpsilon)/(classes.size*classes.size)) else Math.log(defaultEpsilon/(classes.size*classes.size*classes.size))))
        )
      })
    }).toMap
  }

  val humanObservationTypesCache = mutable.HashMap[NodeType, (NodeType, FactorType)]()
  def getHumanObservationTypes(nodeType : NodeType) : (NodeType, FactorType) = {
    if (!humanObservationTypesCache.contains(nodeType)) {
      val humanObservationNodeType = stream.makeNodeType(nodeType.possibleValues)
      val humanObservationFactorType = stream.makeFactorType(List(nodeType, humanObservationNodeType), getInitialHumanErrorGuessWeights(nodeType.possibleValues))
      humanObservationTypesCache.put(nodeType, (humanObservationNodeType, humanObservationFactorType))
    }
    humanObservationTypesCache(nodeType)
  }

  def attachHumanObservation(graph : Graph, node : GraphNode, askHuman : GraphNode => String) = {
    val humanTypes = getHumanObservationTypes(node.nodeType)
    val humanObservationNode = graph.makeNode(humanTypes._1, observedValue = askHuman(node))
    graph.makeFactor(humanTypes._2, List(node, humanObservationNode))
  }

}
