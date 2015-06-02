package edu.stanford.lense_base

import java.util

import edu.stanford.lense_base.gameplaying._
import edu.stanford.lense_base.graph._
import edu.stanford.lense_base.humancompute._
import edu.stanford.lense_base.models.{ModelVariable, Model, ModelStream}
import edu.stanford.lense_base.util.{AsyncBudgetManager, CaseClassEq}
import edu.stanford.lense_base.server._

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Promise, Future}
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by keenon on 4/27/15.
 *
 * This is the central static dispatcher to handle requests to the API
 */
class LenseEngine(stream : ModelStream,
                  initGamePlayer : GamePlayer,
                  humanErrorDistribution : HumanErrorDistribution,
                  humanDelayDistribution : HumanDelayDistribution,
                  var runLearningThread : Boolean = true) {
  val budget : AsyncBudgetManager = new AsyncBudgetManager()

  val pastGuesses = ListBuffer[Model]()

  def getHumanErrorDistribution = humanErrorDistribution
  def getHumanDelayDistribution = humanDelayDistribution

  initGamePlayer.budget = budget
  var gamePlayer = initGamePlayer

  // this is really just for analytics sake, to debug when optimizers are to blame for poor performance
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
  val modelRegularization = 1.0

  // Create a thread to update retrain the weights asynchronously whenever there's an update
  if (runLearningThread) {
    new Thread {
      override def run() = {
        var trainedOnGuesses = 0
        while (runLearningThread) {

          if (pastGuesses.size > trainedOnGuesses) {
            System.err.println("Retraining model")
            // Important to think about how to correctly handle removing regularization over time, or not...
            currentModelLoss = stream.learn(pastGuesses)
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
  }

  def getModelRegularization(dataSize : Int) : Double = {
    modelRegularization
  }

  def predict(model : Model, askHuman : (ModelVariable, HumanComputeUnit) => WorkUnit, hcuPool : HCUPool, lossFunction : (List[(ModelVariable, String, Double)],  Double, Long) => Double, maxLossPerNode : Double) : Promise[(Model, Map[ModelVariable, String], PredictionSummary)] = {
    val promise = Promise[(Model, Map[ModelVariable,String], PredictionSummary)]()
    InFlightPrediction(this, model, budget, askHuman, hcuPool, lossFunction, maxLossPerNode, promise)
    promise
  }

  /**
   * This lets you pass extra training data into the Lense instance, so that it starts out with a better prior than just
   * plain old uniform.
   *
   * @param labels
   */
  def addTrainingData(labels : List[Model]) : Unit = {
    pastGuesses ++= labels
    println("Doing initial learning...")
    stream.learn(pastGuesses)
    println("Finished")
  }
}

/**
 * This is a big nasty class to just manage bundling all the information I need for analytics. It's not particularly
 * pretty, but it's convenient for tunnelling lots of information from where its generated to where its reported.
 */
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
                             humanQueryResponses : List[(ModelVariable, HumanComputeUnit, String)],
                             humanQueryDelays : List[(HumanComputeUnit, Long)],
                             believedVsReceivedHumanDistribution : List[(Map[List[String],Double],Map[List[String],Double])],
                             behaviorLog : String)

case class InFlightPrediction(engine : LenseEngine,
                              model : Model,
                              budget : AsyncBudgetManager,
                              askHuman : (ModelVariable, HumanComputeUnit) => WorkUnit,
                              hcuPool : HCUPool,
                              lossFunction : (List[(ModelVariable, String, Double)], Double, Long) => Double,
                              maxLossPerNode : Double,
                              returnPromise : Promise[(Model, Map[ModelVariable, String], PredictionSummary)]) extends CaseClassEq {
  // Create an initial game state
  var gameState = GameState(model, hcuPool, lossFunction, maxLossPerNode)

  var turnedIn = false
  var initialMove = true

  ////////////////////////////////////
  // Analytics stuff
  ////////////////////////////////////
  var numRequests = 0
  var numRequestsCompleted = 0
  var numRequestsFailed = 0
  var totalCost = 0.0
  var humanResponses = ListBuffer[(ModelVariable, HumanComputeUnit, String)]()
  var humanQueryDelays = ListBuffer[(HumanComputeUnit, Long)]()
  val behaviorLogLock = new Object()
  var behaviorLog = ""
  val confidenceSet = gameState.model.marginals.map(pair => {
    pair._2.maxBy(_._2)._2
  })
  val initialAverageConfidence = confidenceSet.sum / confidenceSet.size
  val initialMinConfidence = confidenceSet.min
  val initialMaxConfidence = confidenceSet.max
  def writeToLog(note : String): Unit = behaviorLogLock.synchronized {
    behaviorLog += note + "\n"
  }
  def logMarginals(): Unit = {
    for (variable <- model.variables) {
      writeToLog("\t"+variable+":")
      writeToLog("\t\t"+gameState.model.marginals(variable))
    }
  }
  ////////////////////////////////////
  // End analytics stuff
  ////////////////////////////////////

  // Make sure that when new humans appear we reasses our gameplaying options
  hcuPool.registerHCUArrivedCallback(this, () => {
    gameplayerMove()
  })

  // Initialize with a move
  gameplayerMove()

  def gameplayerMove() : Unit = this.synchronized {
    // We already turned in this set, this request is being called from some stray delayed call
    if (turnedIn) return

    ////////////////////////////////////
    // Analytics stuff
    if (initialMove) {
      writeToLog("**** BEGIN ****")
      writeToLog("GRAPH:")
      writeToLog(model.toString)
      writeToLog("ORIGINAL MARGINALS:")
      logMarginals()
      initialMove = false
    }
    // End analytics stuff
    ////////////////////////////////////

    System.err.println("Reserved by gameplayer on start of move: $"+budget.reserved.getOrDefault(gameState, 0.0))

    // Force the system to turn in all guesses after it's run out of budget or workers
    val optimalMove = if (budget.budget < 0.001 || hcuPool.hcuPool.size == 0) {
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

        ////////////////////////////////////
        // Analytics stuff
        writeToLog(">> TURNING IN GUESS ("+(System.currentTimeMillis() - gameState.startTime)+")")
        for (variable <- model.variables) {
          writeToLog("\t"+variable+": "+gameState.model.marginals.apply(variable))
        }
        writeToLog("**** END ****")
        // End analytics stuff
        ////////////////////////////////////

        // Return any money we had reserved when considering options
        System.err.println("Turning in guess - returning budget")
        budget.spendReservedBudget(0.0, gameState)

        // Make sure no future gameplayerMoves happen
        turnedIn = true
        hcuPool.removeHCUArrivedCallback(this)

        // Cancel all outstanding requests
        gameState.inFlightRequests.foreach(triple => {
          triple._2.revokeWorkUnit(triple._3)
        })

        // Add this run to our training set
        engine.pastGuesses.synchronized {
          // If we queried at least 2 humans for this one, hopefully minimizes noise in training data
          if (numRequestsCompleted >= 2) {
            engine.pastGuesses += gameState.model
            // Wake up the parallel weights trainer:
            engine.pastGuesses.notifyAll()
          }
        }

        // Complete the query promise, returning the async request to us with a value
        returnPromise.complete(Try {
            (gameState.model, gameState.model.map,
              ////////////////////////////////////
              // Analytics stuff
              {
                val model = gameState.model
                val valueSets = model.variables.map(_.possibleValues).distinct

                PredictionSummary(gameState.loss(),
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
                  valueSets.map(
                    set => (engine.getHumanErrorDistribution.enumerateBelievedProbabilities(set), engine.getHumanErrorDistribution.enumerateReceivedProbabilities(set))
                  ),
                  behaviorLog)
              }
              // End analytics stuff
              ////////////////////////////////////
            )
          })
      case obs : MakeHumanObservation =>

        ////////////////////////////////////
        // Analytics stuff
        writeToLog(">> MAKE HUMAN OBSERVATION "+obs.variable+" (time "+(System.currentTimeMillis() - gameState.startTime)+")")
        writeToLog("\tAsking "+obs.hcu+" about "+obs.variable)
        // End analytics stuff
        ////////////////////////////////////

        // Commit the engine to actually spending the money, and return anything we didn't use
        System.err.println("Making human observation budget spend")
        budget.spendReservedBudget(obs.hcu.cost, gameState)

        // Create a new work unit
        val workUnit = askHuman(obs.variable, obs.hcu)

        println("Asking human about "+obs.variable)

        numRequests += 1

        val launchedObservation : Long = System.currentTimeMillis()

        // When the work unit returns, do the following
        workUnit.promise.future.onComplete(t => {
          this.synchronized {
            if (t.isSuccess) {
              // If the workUnit succeeded, move the gamestate
              gameState = gameState.getNextStateForVariableObservation(obs.variable, obs.hcu, workUnit, t.get)
              numRequestsCompleted += 1
              totalCost += obs.hcu.cost

              ////////////////////////////////////
              // Analytics stuff
              println("Received response for "+obs.variable+": "+t.get)
              writeToLog(">> RECEIVED HUMAN RESPONSE "+obs.variable+"="+t.get+" (time "+(System.currentTimeMillis() - gameState.startTime)+")")
              writeToLog("\tFrom "+obs.hcu+": got "+obs.variable+"="+t.get)
              writeToLog("UPDATED MARGINALS:")
              logMarginals()
              // End analytics stuff
              ////////////////////////////////////

              humanResponses.+=((obs.variable, obs.hcu, t.get))
              humanQueryDelays.+=((obs.hcu, System.currentTimeMillis() - launchedObservation))
            }
            else {

              ////////////////////////////////////
              // Analytics stuff
              writeToLog(">> HUMAN RESPONSE FAILED (time "+(System.currentTimeMillis() - gameState.startTime)+")")
              writeToLog("\tTime: "+(System.currentTimeMillis() - gameState.startTime))
              writeToLog("\tFrom "+obs.hcu)
              System.err.println("Workunit Failed! "+t.failed.get.getMessage)
              // End analytics stuff
              ////////////////////////////////////

              // If the workUnit failed, then fail appropriately
              gameState = gameState.getNextStateForFailedRequest(obs.variable, obs.hcu, workUnit)
              numRequestsFailed += 1
            }
          }
          // On every change we should recurse
          gameplayerMove()
        })
        gameState = gameState.getNextStateForInFlightRequest(obs.variable, obs.hcu, workUnit, System.currentTimeMillis())
        // Move again immediately
        gameplayerMove()

      case WaitForTime(delay) =>
        ////////////////////////////////////
        // Analytics stuff
        writeToLog(">> WAIT ("+(System.currentTimeMillis() - gameState.startTime)+")")
        System.err.println("Waiting for "+delay+" - returning budget")
        // End analytics stuff
        ////////////////////////////////////

        budget.spendReservedBudget(0.0, gameState)
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
        ////////////////////////////////////
        // Analytics stuff
        writeToLog(">> WAIT ("+(System.currentTimeMillis() - gameState.startTime)+")")
        System.err.println("Waiting - returning budget")
        // End analytics stuff
        ////////////////////////////////////

        budget.spendReservedBudget(0.0, gameState)
        // Do nothing, for now. Wait for stimulus
    }
  }
}

