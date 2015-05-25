package edu.stanford.lense_base

import edu.stanford.lense_base.gameplaying._
import edu.stanford.lense_base.graph._
import edu.stanford.lense_base.humancompute.{WorkUnit, HumanComputeUnit, HCUPool}
import edu.stanford.lense_base.util.CaseClassEq
import edu.stanford.lense_base.server._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Promise, Future}
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

import java.util.IdentityHashMap

/**
 * Created by keenon on 4/27/15.
 *
 * This is the central static dispatcher to handle requests to the API
 */
class LenseEngine(stream : GraphStream,
                  initGamePlayer : GamePlayer,
                  humanErrorDistribution : HumanErrorDistribution,
                  humanDelayDistribution : HumanDelayDistribution) {
  val defaultHumanErrorEpsilon = 0.3

  val pastGuesses = mutable.ListBuffer[Graph]()
  val pastQueryStructure = mutable.ListBuffer[Graph]()

  // This is a safety stop, where runs can be given specific budgets that they are not intended to cross.
  var budget = 0.0
  var reserved = new IdentityHashMap[Any, Double]()
  var spent = 0.0

  val budgetLock = new Object()

  def getHumanErrorDistribution = humanErrorDistribution
  def getHumanDelayDistribution = humanDelayDistribution

  def addBudget(amount : Double) = budgetLock.synchronized {
    budget += amount
  }

  def tryReserveBudget(amount : Double, owner : Any, hcuPool : HCUPool) : Boolean = budgetLock.synchronized {
    if (reserved.getOrDefault(owner, 0.0) > 0.0) throw new IllegalStateException("The same reserver shouldn't take more than one reservation at a time")
    if (budget >= amount) {
      budget -= amount
      reserved.put(owner, amount)
      System.err.println("Reserving amount $"+amount)
      System.err.println("Left-over budget: "+budget)
      true
    }
    else {
      // If we ever hit around 0, then send away all our workers, because we have no more money to pay them with...
      if (budget < 0.001) {
        for (hcu <- hcuPool.hcuPool) {
          hcu match {
            case client : HCUClient =>
              client.completeAndPay()
            case _ =>
          }
        }
      }
      false
    }
  }

  def spendReservedBudget(amount : Double, owner : Any, hcuPool : HCUPool) : Unit = budgetLock.synchronized {
    if (amount == 0) {
      if (reserved.containsKey(owner)) {
        budget += reserved.get(owner)
        System.err.println("Reclaimed reserve $"+reserved.get(owner)+", budget remaining unclaimed: $"+budget)
        reserved.put(owner, 0.0)
      }
    }
    else {
      if (reserved.get(owner) >= amount) {
        val totalReserved = reserved.get(owner)
        val amountRemaining = totalReserved - amount
        budget += amountRemaining
        reserved.put(owner, 0.0)

        System.err.println("Spent $"+amount+", reclaimed $"+amountRemaining+", budget remaining unclaimed: $"+budget)
      }
      else {
        throw new IllegalStateException("Trying to spend budget that was never properly reserved")
      }
    }
  }

  initGamePlayer.engine = this
  var gamePlayer = initGamePlayer

  var runLearningThread = true

  var currentModelLoss = 0.0

  def currentLoss() : Double = {
    currentModelLoss
  }

  // You probably don't want to call this.
  // This turns off the learning thread, and will prevent your model from updating as it gains experience
  def turnOffLearning() : Unit = {
    runLearningThread = false
    // Wake up the thread, so it can die
    pastGuesses.synchronized {
      pastGuesses.notify()
    }
  }

  var numSwapsSoFar = 0
  val modelRegularization = 0.5

  // Create a thread to update retrain the weights asynchronously whenever there's an update
  new Thread {
    override def run() = {
      var trainedOnGuesses = 0
      while (runLearningThread) {
        if (pastGuesses.size > trainedOnGuesses) {
          System.err.println("Retraining model")
          // Important to think about how to correctly handle removing regularization over time, or not...
          learnHoldingPastGuessesConstant(getModelRegularization(pastGuesses.size))
          System.err.println("Hot swapping model")
          numSwapsSoFar += 1
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

  def getModelRegularization(dataSize : Int) : Double = {
    modelRegularization / Math.log(dataSize)
  }

  def predict(graph : Graph, askHuman : (GraphNode, HumanComputeUnit) => WorkUnit, hcuPool : HCUPool, lossFunction : (List[(GraphNode, String, Double)], Double, Long) => Double) : Promise[(Map[GraphNode, String], PredictionSummary)] = {
    val promise = Promise[(Map[GraphNode,String], PredictionSummary)]()
    InFlightPrediction(this, graph, askHuman, hcuPool, lossFunction, promise)
    promise
  }

  def learnHoldingPastGuessesConstant(l2regularization : Double = getModelRegularization(pastGuesses.size)) = this.synchronized {
    // Keep the old optimizer, because we want the accumulated history, since we've hardly changed the function at all
    currentModelLoss = stream.learn(pastGuesses, l2regularization, clearOptimizer = true)
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
          Map[String,Double]("BIAS" -> Math.log(humanErrorDistribution.jointProbability(cl1, cl2)))
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
    learnHoldingPastGuessesConstant(getModelRegularization(pastGuesses.size))
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
                             initialAvgConfidence : Double,
                             numSwapsSoFar : Int,
                             modelTrainingLoss : Double,
                             numExamplesSeen : Int,
                             humanQueryResponses : List[(GraphNode, HumanComputeUnit, String)],
                             humanQueryDelays : List[(HumanComputeUnit, Long)],
                             behaviorLog : String)

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

  var humanResponses = ListBuffer[(GraphNode, HumanComputeUnit, String)]()
  var humanQueryDelays = ListBuffer[(HumanComputeUnit, Long)]()

  var behaviorLog = ""

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

    System.err.println("Reserved by gameplayer on start of move: $"+engine.reserved.getOrDefault(engine.gamePlayer, 0.0))

    // Force the system to turn in all guesses after it's run out of budget or workers
    val optimalMove = if (engine.budget < 0.001 || hcuPool.hcuPool.size == 0) {
      for (hcu <- hcuPool.hcuPool) {
        hcu match {
          case client : HCUClient =>
            client.completeAndPay()
          case _ =>
        }
      }
      TurnInGuess()
    }
    // If it still has budget and workers, then get an optimal move
    else engine.gamePlayer.getOptimalMove(gameState)

    optimalMove match {
      case _ : TurnInGuess =>
        // Return any money we had reserved when considering options
        System.err.println("Turning in guess - returning budget")
        engine.spendReservedBudget(0.0, gameState, hcuPool)

        // Make sure no future gameplayerMoves happen
        turnedIn = true
        hcuPool.removeHCUArrivedCallback(this)

        val mapEstimate = gameState.graph.mapEstimate()

        // Cancel all outstanding requests
        gameState.inFlightRequests.foreach(triple => {
          triple._2.revokeWorkUnit(triple._3)
        })

        engine.pastGuesses.synchronized {
          // If we queried at least 2 humans for this one, hopefully minimizes noise in training data
          if (gameState.graph.nodes.size > gameState.originalGraph.nodes.size + 1) {
            // Store the original request graph, with our guessed labels, in our pastGuesses stream
            // Learning from this should be convex, so provide at least a good initialization for later values
            val toStoreGraphPair = gameState.originalGraph.clone()
            val toStoreGraph = toStoreGraphPair._1
            val toStoreOldToNew = toStoreGraphPair._2

            gameState.originalGraph.nodes.foreach(n => {
              toStoreOldToNew(n).observedValue = mapEstimate(gameState.oldToNew(n))
            })
            engine.pastGuesses += toStoreGraph
          }
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
              initialAverageConfidence,
              engine.numSwapsSoFar,
              engine.currentLoss(),
              engine.pastGuesses.size,
              humanResponses.toList,
              humanQueryDelays.toList,
              behaviorLog))
          })
      case obs : MakeHumanObservation =>
        // Commit the engine to actually spending the money, and return anything we didn't use
        System.err.println("Making human observation budget spend")
        engine.spendReservedBudget(obs.hcu.cost, gameState, hcuPool)

        // Create a new work unit
        val workUnit = askHuman(obs.node, obs.hcu)

        println("Asking human about "+obs.node)

        numRequests += 1

        val launchedObservation : Long = System.currentTimeMillis()

        // When the work unit returns, do the following
        workUnit.promise.future.onComplete(t => {
          this.synchronized {
            if (t.isSuccess) {
              // If the workUnit succeeded, move the gamestate
              println("Received response for "+obs.node+": "+t.get)
              gameState = gameState.getNextStateForNodeObservation(obs.node, obs.hcu, workUnit, t.get)
              numRequestsCompleted += 1
              totalCost += obs.hcu.cost

              humanResponses.+=((obs.node, obs.hcu, t.get))
              humanQueryDelays.+=((obs.hcu, System.currentTimeMillis() - launchedObservation))
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

      case WaitForTime(delay) =>
        System.err.println("Waiting for "+delay+" - returning budget")
        engine.spendReservedBudget(0.0, gameState, hcuPool)
        val closureGameState = gameState
        new Thread(new Runnable {
          override def run(): Unit = {
            Thread.sleep(delay)
            // Only wake up if we weren't woken up before our deadline
            if (gameState eq closureGameState) {
              gameplayerMove()
            }
          }
        }).start()

      case wait : Wait =>
        System.err.println("Waiting - returning budget")
        engine.spendReservedBudget(0.0, gameState, hcuPool)
        // Do nothing, for now. Wait for stimulus
    }
  }
}

