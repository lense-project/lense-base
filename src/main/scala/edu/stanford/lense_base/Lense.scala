package edu.stanford.lense_base

import edu.stanford.lense_base.gameplaying.{MakeHumanObservation, TurnInGuess, GameState, GamePlayer}
import edu.stanford.lense_base.graph._

import scala.collection.mutable

/**
 * Created by keenon on 4/27/15.
 *
 * This is the central static dispatcher to handle requests to the API
 */
class Lense(stream : GraphStream, gamePlayer : GamePlayer) {
  val defaultEpsilon = 0.3

  val pastGuesses = mutable.ListBuffer[Graph]()
  val pastQueryStructure = mutable.ListBuffer[Graph]()

  def predict(graph : Graph, askHuman : GraphNode => String, lossFunction : (List[(GraphNode, String, Double)], Double, Double) => Double) : Map[GraphNode, String] = {
    var gameState = GameState(graph, 0.0, 0.0, askHuman, attachHumanObservation, lossFunction)

    if (pastGuesses.size > 0) {
      println("Learning...")
      learnHoldingPastGuessesConstant()
      println("Finished Learning:")
      println(stream.withDomainList)
    }

    // Keep playing until the game player tells us to stop

    while (true) {
      val optimalMove = gamePlayer.getOptimalMove(gameState)
      optimalMove match {
        case _ : TurnInGuess =>
          val mapEstimate = gameState.graph.mapEstimate()

          // Store the original request graph, with our guessed labels, in our pastGuesses stream
          gameState.originalGraph.nodes.foreach(n => {
            n.observedValue = mapEstimate(gameState.oldToNew(n))
          })
          pastGuesses += gameState.originalGraph
          // Store the uncertainty, with all human queries attached, in pastQueryStructure stream
          pastQueryStructure += gameState.graph

          return mapEstimate
        case obs : MakeHumanObservation =>
          gameState = gameState.takeRealMove(obs)
      }
    }

    // Code will never reach here, because the only way to exit while() loop is to TurnInGuess
    throw new IllegalStateException("Code should never reach this point")
  }

  def learnHoldingPastGuessesConstant(regularization : Double = 1.0) = {
    try {
      stream.learn(pastGuesses, regularization)
    }
    catch {
      case e : Throwable =>
        e.printStackTrace()
    }
    // Reset human weights to default, because regularizer will have messed with them
    for (humanObservationTypePair <- humanObservationTypesCache.values) {
      humanObservationTypePair._2.setWeights(getInitialHumanErrorGuessWeights(humanObservationTypePair._1.possibleValues))
    }
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

  def attachHumanObservation(graph : Graph, node : GraphNode, humanOpinion : String) : Unit = {
    val humanTypes = getHumanObservationTypes(node.nodeType)
    val humanObservationNode = graph.makeNode(humanTypes._1, observedValue = humanOpinion)
    graph.makeFactor(humanTypes._2, List(node, humanObservationNode))
  }

}
