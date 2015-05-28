package edu.stanford.lense_base.gameplaying

/**
 * Created by keenon on 5/21/15.
 *
 * This just runs N human queries per node, and then turns in the result
 */
class NQuestionBaseline(n : Int) extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    getAllLegalMoves(state, reserveRealBudget = true)

    for (node <- state.originalGraph.nodes) {
      val numReqs = state.completedRequests.count(_._1 eq node) + state.inFlightRequests.count(_._1 eq node)
      if (numReqs < n) {
        if (state.hcuPool.hcuPool.size > 0) {
          return MakeHumanObservation(node, state.hcuPool.hcuPool.minBy(hcu => hcu.estimateRequiredTimeIncludingQueue(node)))
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
