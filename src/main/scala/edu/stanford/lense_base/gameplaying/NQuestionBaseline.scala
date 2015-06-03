package edu.stanford.lense_base.gameplaying

/**
 * Created by keenon on 5/21/15.
 *
 * This just runs N human queries per node, and then turns in the result
 */
class NQuestionBaseline(n : Int) extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    if (state.hcuPool.hcuPool.size == 0) return Wait()

    val legalMoves = getAllLegalMoves(state, reserveRealBudget = true)

    // Ask everything we legally can first
    for (variable <- state.model.variables) {
      val numReqs = state.completedRequests.count(_._1 eq variable) + state.inFlightRequests.count(_._1 eq variable)
      if (numReqs < n) {
        val legalMovesOnThisVariable = legalMoves.filter(_.isInstanceOf[MakeHumanObservation]).map(_.asInstanceOf[MakeHumanObservation]).filter(_.variable == variable)
        if (legalMovesOnThisVariable.size > 0) {
          return legalMovesOnThisVariable.minBy(move => move.hcu.estimateRequiredTimeIncludingQueue(variable))
        }
      }
    }

    // Wait only if we've exhausted all other legal moves
    for (variable <- state.model.variables) {
      val numReqs = state.completedRequests.count(_._1 eq variable) + state.inFlightRequests.count(_._1 eq variable)
      if (numReqs < n) {
        val legalMovesOnThisVariable = legalMoves.filter(_.isInstanceOf[MakeHumanObservation]).map(_.asInstanceOf[MakeHumanObservation]).filter(_.variable == variable)
        if (legalMovesOnThisVariable.size == 0) return Wait()
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
