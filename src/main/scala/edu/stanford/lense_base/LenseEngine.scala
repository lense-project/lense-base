package edu.stanford.lense_base

import edu.stanford.lense_base.gameplaying._
import edu.stanford.lense_base.graph._
import edu.stanford.lense_base.humancompute.{WorkUnit, HumanComputeUnit, HCUPool}
import edu.stanford.lense_base.util.CaseClassEq

import scala.collection.mutable
import scala.concurrent.{Promise, Future}
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by keenon on 4/27/15.
 *
 * This is the central static dispatcher to handle requests to the API
 */
class LenseEngine(stream : GraphStream, initGamePlayer : GamePlayer) {
  val defaultHumanErrorEpsilon = 0.3

  val pastGuesses = mutable.ListBuffer[Graph]()
  val pastQueryStructure = mutable.ListBuffer[Graph]()

  var gamePlayer = initGamePlayer

  var runLearningThread = true

  // You probably don't want to call this.
  // This turns off the learning thread, and will prevent your model from updating as it gains experience
  def turnOffLearning() : Unit = {
    runLearningThread = false
    // Wake up the thread, so it can die
    pastGuesses.synchronized {
      pastGuesses.notify()
    }
  }

  // Create a thread to update retrain the weights asynchronously whenever there's an update
  new Thread {
    override def run() = {
      var trainedOnGuesses = 0
      while (runLearningThread) {
        if (pastGuesses.size > trainedOnGuesses) {
          System.err.println("Retraining model")
          learnHoldingPastGuessesConstant(10.0)
          System.err.println("Hot swapping model")
          trainedOnGuesses = pastGuesses.size
        }
        else {
          pastGuesses.synchronized {
            pastGuesses.wait()
          }
        }
      }
    }
  }.start()

  def predict(graph : Graph, askHuman : (GraphNode, HumanComputeUnit) => WorkUnit, hcuPool : HCUPool, lossFunction : (List[(GraphNode, String, Double)], Double, Long) => Double) : Promise[(Map[GraphNode, String], PredictionSummary)] = {
    val promise = Promise[(Map[GraphNode,String], PredictionSummary)]()
    InFlightPrediction(this, graph, askHuman, hcuPool, lossFunction, promise)
    promise
  }

  def learnHoldingPastGuessesConstant(regularization : Double = 1.0) = this.synchronized {
    // Keep the old optimizer, because we want the accumulated history, since we've hardly changed the function at all
    stream.learn(pastGuesses, regularization, clearOptimizer = true)
    // Reset human weights to default, because regularizer will have messed with them
    for (humanObservationTypePair <- humanObservationTypesCache.values) {
      humanObservationTypePair._2.setWeights(getInitialHumanErrorGuessWeights(humanObservationTypePair._1.possibleValues))
    }
  }

  def onlineUpdateHoldingPastGuessesConstant(graph : Graph, regularization : Double = 1.0) = this.synchronized {
    stream.onlineUpdate(List(graph), regularization)
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
          Map[String,Double]("BIAS" -> (if (cl1 == cl2) Math.log((1-defaultHumanErrorEpsilon)/(classes.size*classes.size)) else Math.log(defaultHumanErrorEpsilon/(classes.size*classes.size*classes.size))))
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

  /**
   * This lets you pass extra training data into the Lense instance, so that it starts out with a better prior than just
   * plain old uniform.
   *
   * @param labels
   */
  def addTrainingData(labels : List[Graph]) : Unit = {
    pastGuesses ++= labels
    println("Doing initial learning...")
    learnHoldingPastGuessesConstant()
    println("Finished")
  }
}

case class PredictionSummary(loss : Double,
                             numRequests : Int,
                             numRequestsCompleted : Int,
                             numRequestsFailed : Int,
                             requestCost : Double,
                             timeRequired : Long,
                             initialMinConfidence : Double,
                             initialMaxConfidence : Double,
                             initialAvgConfidence : Double)

case class InFlightPrediction(engine : LenseEngine,
                              originalGraph : Graph,
                              askHuman : (GraphNode, HumanComputeUnit) => WorkUnit,
                              hcuPool : HCUPool,
                              lossFunction : (List[(GraphNode, String, Double)], Double, Long) => Double,
                              returnPromise : Promise[(Map[GraphNode, String], PredictionSummary)]) extends CaseClassEq {
  // Create an initial game state
  var gameState = GameState(originalGraph, 0.0, hcuPool, engine.attachHumanObservation, lossFunction)

  var turnedIn = false

  var numRequests = 0
  var numRequestsCompleted = 0
  var numRequestsFailed = 0
  var totalCost = 0.0

  // Make sure that when new humans appear we reasses our gameplaying options
  hcuPool.registerHCUArrivedCallback(this, () => {
    gameplayerMove()
  })

  val confidenceSet = originalGraph.marginalEstimate().map(pair => {
    pair._2.maxBy(_._2)._2
  })
  val initialAverageConfidence = confidenceSet.sum / confidenceSet.size
  val initialMinConfidence = confidenceSet.min
  val initialMaxConfidence = confidenceSet.max

  // Initialize with a move
  gameplayerMove()

  def gameplayerMove() : Unit = this.synchronized {
    // We already turned in this set, this request is being called from some stray delayed call
    if (turnedIn) return

    val optimalMove = engine.gamePlayer.getOptimalMove(gameState)

    optimalMove match {
      case _ : TurnInGuess =>
        // Make sure no future gameplayerMoves happen
        turnedIn = true
        hcuPool.removeHCUArrivedCallback(this)

        val mapEstimate = gameState.graph.mapEstimate()

        // Cancel all outstanding requests
        gameState.inFlightRequests.foreach(triple => {
          triple._2.revokeWorkUnit(triple._3)
        })

        // Store the original request graph, with our guessed labels, in our pastGuesses stream
        // Learning from this should be convex, so provide at least a good initialization for later values
        gameState.originalGraph.nodes.foreach(n => {
          n.observedValue = mapEstimate(gameState.oldToNew(n))
        })

        engine.pastGuesses.synchronized {
          engine.pastGuesses += gameState.originalGraph
          engine.pastQueryStructure += gameState.graph
          // Wake up the parallel weights trainer:
          engine.pastGuesses.notifyAll()
        }

        returnPromise.complete(Try {
            (mapEstimate.map(pair => {
              val matches = gameState.originalGraph.nodes.filter(n => gameState.oldToNew(n) eq pair._1)
              if (matches.size != 1) throw new IllegalStateException("Bad oldToNew mapping")
              (matches(0), pair._2)
            }), PredictionSummary(gameState.loss(),
              numRequests,
              numRequestsCompleted,
              numRequestsFailed,
              totalCost,
              System.currentTimeMillis() - gameState.startTime,
              initialMinConfidence,
              initialMaxConfidence,
              initialAverageConfidence))
          })
      case obs : MakeHumanObservation =>
        // Create a new work unit
        val workUnit = askHuman(obs.node, obs.hcu)

        println("Asking human about "+obs.node)

        numRequests += 1

        // When the work unit returns, do the following
        workUnit.promise.future.onComplete(t => {
          this.synchronized {
            if (t.isSuccess) {
              // If the workUnit succeeded, move the gamestate
              println("Received response for "+obs.node+": "+t.get)
              gameState = gameState.getNextStateForNodeObservation(obs.node, obs.hcu, workUnit, t.get)
              numRequestsCompleted += 1
              totalCost += obs.hcu.cost
            }
            else {
              System.err.println("Workunit Failed! "+t.failed.get.getMessage)
              // If the workUnit failed, then fail appropriately
              gameState = gameState.getNextStateForFailedRequest(obs.node, obs.hcu, workUnit)
              numRequestsFailed += 1
            }
          }
          // On every change we should recurse
          gameplayerMove()
        })
        gameState = gameState.getNextStateForInFlightRequest(obs.node, obs.hcu, workUnit)
        // Move again immediately
        gameplayerMove()

      case wait : Wait =>
        // Do nothing, for now. Wait for stimulus
    }
  }
}