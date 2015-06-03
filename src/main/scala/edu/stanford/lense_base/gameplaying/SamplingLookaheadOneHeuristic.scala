package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.humancompute.{HumanComputeUnit, HumanDelayDistribution, HumanErrorDistribution}
import edu.stanford.lense_base.models.{Model, ModelVariable}

/**
 * Created by keenon on 6/1/15.
 *
 * Performs a look-ahead one by sampling possible outcomes of in flight requests, and the loss we would incur in each
 * case
 */
class SamplingLookaheadOneHeuristic(humanErrorDistribution : HumanErrorDistribution, humanDelayDistribution : HumanDelayDistribution) extends GamePlayer {

  val SAMPLES_TO_TAKE = 5

  def sampleLossForInFlight(gameState : GameState, inFlightList : List[(ModelVariable, Long, HumanComputeUnit)]) : Double = {
    var loss = 0.0

    val initialMarginals = gameState.model.marginals

    for (i <- 0 to SAMPLES_TO_TAKE) {
      var currentState = gameState
      var delay = 0L
      for (pair <- inFlightList) {
        val sampledResponse = humanErrorDistribution.sampleGivenMarginals(initialMarginals(pair._1))
        val sampledDelay = humanDelayDistribution.sampleDelay()
        if (sampledDelay > delay) delay = sampledDelay
        currentState = currentState.getNextStateForVariableObservation(pair._1, pair._3, null, sampledResponse)
      }

      loss += currentState.loss(delay)
    }

    loss / SAMPLES_TO_TAKE
  }

  def estimateLossForMove(move : GameMove, state : GameState) : Double = {
    val currentlyInFlight = state.inFlightRequests.map(quad => (quad._1, quad._4, quad._2)).toList
    move match {
      case _ : TurnInGuess => state.loss()
      case _ : Wait =>
        sampleLossForInFlight(state, currentlyInFlight)
      case MakeHumanObservation(variable, hcu) =>
        sampleLossForInFlight(state, currentlyInFlight :+ (variable, 0L, hcu))
    }
  }

  override def getOptimalMove(state: GameState): GameMove = {
    // This prevents ripping through the data when all our Turkers go home
    if (state.hcuPool.hcuPool.size == 0) return Wait()

    val legalMoves = getAllLegalMoves(state)

    // Don't both calculating expectations if there's only one choice anyways
    if (legalMoves.size == 1) return legalMoves.head

    // Calculate loss for each move in its own thread. In theory this will offer a reasonable speedup, delivering
    // faster move times.
    case class EstimationThread(move : GameMove) extends Runnable {
      var loss = 0.0
      override def run(): Unit = {
        Thread.currentThread().setPriority(Thread.MAX_PRIORITY)
        loss = estimateLossForMove(move, state)
      }
    }
    val estimators = legalMoves.map(move => {
      new EstimationThread(move)
    })
    val threads = estimators.map(est => new Thread(est))

    val startTime = System.currentTimeMillis()
    threads.foreach(_.start())
    threads.foreach(_.join())

    val movesWithEstimatedLoss = estimators.map(est => (est.move, est.loss))

    println("Estimated losses in "+(System.currentTimeMillis() - startTime)+":")
    println(movesWithEstimatedLoss.map("\t"+_.toString()).mkString("\n"))

    movesWithEstimatedLoss.minBy(_._2)._1
  }
}
