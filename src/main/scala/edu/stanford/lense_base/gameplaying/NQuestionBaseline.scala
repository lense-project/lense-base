package edu.stanford.lense_base.gameplaying

/**
 * Created by keenon on 5/21/15.
 *
 * This just runs N human queries per node, and then turns in the result
 */
class NQuestionBaseline(n : Int) extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    getAllLegalMoves(state, reserveRealBudget = true)

    for (variable <- state.model.variables) {
      val numReqs = state.completedRequests.count(_._1 eq variable) + state.inFlightRequests.count(_._1 eq variable)
      if (numReqs < n) {
        if (state.hcuPool.hcuPool.size > 0) {
          return MakeHumanObservation(variable, state.hcuPool.hcuPool.minBy(hcu => hcu.estimateRequiredTimeIncludingQueue(variable)))
        }
        else {
          return Wait()
        }
      }
    }

    if (state.inFlightRequests.size == 0) {
      TurnInGuess()
    }
    else {
      Wait()
    }
  }
}

object OneQuestionBaseline extends NQuestionBaseline(1)
