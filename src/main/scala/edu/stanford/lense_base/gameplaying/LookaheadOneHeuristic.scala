package edu.stanford.lense_base.gameplaying

/**
 * Created by keenon on 5/21/15.
 *
 * This game player calculates loss if we turn in the solution now, and expected loss if we turn in after each possible query we
 * could make, and takes the best possible
 */

object LookaheadOneHeuristic extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    println("Getting optimal move")

    var optimal = getAllLegalMoves(state).map(m => {
      val nextStates = state.getNextStates(m)
      // get loss now
      val moveLoss =
        if (nextStates.size == 0) state.loss()
        else {
          // get expected loss at next state
          nextStates.map(p => {
            p._1 * p._2.loss()
          }).sum
        }
      (moveLoss, m)
    }).minBy(_._1)

    if (optimal._2.isInstanceOf[TurnInGuess]) {
      if (state.inFlightRequests.size > 0) {
        optimal = (optimal._1, Wait())
      }
    }

    println(optimal)

    optimal._2
  }
}

