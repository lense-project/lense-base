package edu.stanford.lense_base

import edu.stanford.lense_base.gameplaying._
import edu.stanford.lense_base.graph._

import scala.collection.mutable
import scala.concurrent.{Promise, Future}

/**
 * Created by keenon on 4/27/15.
 *
 * This is the central static dispatcher to handle requests to the API
 */
class Lense(stream : GraphStream, gamePlayer : GamePlayer) {
  val defaultEpsilon = 0.3

  val pastGuesses = mutable.ListBuffer[Graph]()
  val pastQueryStructure = mutable.ListBuffer[Graph]()

  val pastGameTrajectories = mutable.ListBuffer[List[(GameState,GameMove)]]()

  def predict(graph : Graph, askHuman : GraphNode => Promise[String], lossFunction : (List[(GraphNode, String, Double)], Double, Double) => Double) : Map[GraphNode, String] = {
    var gameState = GameState(graph, 0.0, 0.0, askHuman, attachHumanObservation, lossFunction)

    if (pastGuesses.size > 0) {
      println("Learning...")
      learnHoldingPastGuessesConstant()
      println("Finished Learning:")
    }

    // Keep playing until the game player tells us to stop

    val gameTrajectory = mutable.ListBuffer[(GameState,GameMove)]()
    while (true) {
      val optimalMove = gamePlayer.getOptimalMove(gameState)

      // Add this move to our recorded trajectory
      gameTrajectory += ((gameState, optimalMove))

      optimalMove match {
        case _ : TurnInGuess =>
          val mapEstimate = gameState.graph.mapEstimate()

          // Store the original request graph, with our guessed labels, in our pastGuesses stream
          // Learning from this should be convex, so provide at least a good initialization for later values
          gameState.originalGraph.nodes.foreach(n => {
            n.observedValue = mapEstimate(gameState.oldToNew(n))
          })
          pastGuesses += gameState.originalGraph

          // Store the uncertainty, with all human queries attached, in pastQueryStructure stream
          // Learning from this will require learning with unobserved variables, so will be subject to local optima
          pastQueryStructure += gameState.graph

          // Store the game trajectory for debugging and analysis
          pastGameTrajectories += gameTrajectory.clone().toList
          gameTrajectory.clear()

          return mapEstimate
        case obs : MakeHumanObservation =>
          gameState = gameState.takeRealMove(obs)
      }
    }

    // Code will never reach here, because the only way to exit while() loop is to TurnInGuess
    throw new IllegalStateException("Code should never reach this point")
  }

  def learnHoldingPastGuessesConstant(regularization : Double = 1.0) = {
    stream.learn(pastGuesses, regularization)
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
