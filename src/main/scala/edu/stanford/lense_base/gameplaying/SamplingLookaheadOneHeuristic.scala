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

  val SAMPLES_TO_TAKE = 15

  def sampleLossForInFlight(gameState : GameState, inFlightList : List[(ModelVariable, Long, HumanComputeUnit)]) : Double = {
    var loss = 0.0

    val localMarginals = gameState.model.marginals
    for (i <- 0 to SAMPLES_TO_TAKE) {
      var currentState = gameState
      var delay = 0L
      for (pair <- inFlightList) {
        val sampledResponse = humanErrorDistribution.sampleGivenMarginals(localMarginals(pair._1))
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
    val legalMoves = getAllLegalMoves(state)

    // Don't both calculating expectations if there's only one choice anyways
    if (legalMoves.size == 1) return legalMoves.head

    val movesWithEstimatedLoss = legalMoves.par.map(move => {
      (move, estimateLossForMove(move, state))
    })

    println("Estimated losses:")
    println(movesWithEstimatedLoss.map("\t"+_.toString()).mkString("\n"))

    movesWithEstimatedLoss.minBy(_._2)._1
  }
}
