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

  def gamePlayer = initGamePlayer

  // Create a thread to update retrain the weights asynchronously whenever there's an update
  new Thread{
    override def run() = {
      var trainedOnGuesses = 0
      while (true) {
        if (pastGuesses.size > trainedOnGuesses) {
          learnHoldingPastGuessesConstant(1.0)
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

  def predict(graph : Graph, askHuman : (GraphNode, HumanComputeUnit) => WorkUnit, hcuPool : HCUPool, lossFunction : (List[(GraphNode, String, Double)], Double, Long) => Double) : Promise[Map[GraphNode, String]] = {
    val promise = Promise[Map[GraphNode,String]]()
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

case class InFlightPrediction(engine : LenseEngine,
                              originalGraph : Graph,
                              askHuman : (GraphNode, HumanComputeUnit) => WorkUnit,
                              hcuPool : HCUPool,
                              lossFunction : (List[(GraphNode, String, Double)], Double, Long) => Double,
                              returnPromise : Promise[Map[GraphNode, String]]) extends CaseClassEq {
  // Create an initial game state
  var gameState = GameState(originalGraph, 0.0, hcuPool, engine.attachHumanObservation, lossFunction)

  var turnedIn = false

  // Make sure that when new humans appear we reasses our gameplaying options
  hcuPool.registerHCUArrivedCallback(this, () => {
    gameplayerMove()
  })

  // Initialize with a move
  gameplayerMove()

  def gameplayerMove() : Unit = this.synchronized {
    println("Attempting a gameplayer Move")
    // We already turned in this set, this request is being called from some stray delayed call
    if (turnedIn) return

    val optimalMove = engine.gamePlayer.getOptimalMove(gameState)

    optimalMove match {
      case _ : TurnInGuess =>
        println("Turning in guess")
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

        println("Completing promise!")
        returnPromise.complete(Try {
            mapEstimate.map(pair => {
              val matches = gameState.originalGraph.nodes.filter(n => gameState.oldToNew(n) eq pair._1)
              if (matches.size != 1) throw new IllegalStateException("Bad oldToNew mapping")
              (matches(0), pair._2)
            })
          })
      case obs : MakeHumanObservation =>
        println("Making human observation")
        // Create a new work unit
        val workUnit = askHuman(obs.node, obs.hcu)

        // When the work unit returns, do the following
        workUnit.promise.future.onComplete(t => {
          this.synchronized {
            if (t.isSuccess) {
              // If the workUnit succeeded, move the gamestate
              println("Workunit Success!")
              gameState = gameState.getNextStateForNodeObservation(obs.node, obs.hcu, workUnit, t.get)
            }
            else {
              println("Workunit Failed!")
              t.failed.get.printStackTrace()
              // If the workUnit failed, then fail appropriately
              println("Pre in flight requests: "+gameState.inFlightRequests)
              gameState = gameState.getNextStateForFailedRequest(obs.node, obs.hcu, workUnit)
              println("Post in flight requests: "+gameState.inFlightRequests)
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