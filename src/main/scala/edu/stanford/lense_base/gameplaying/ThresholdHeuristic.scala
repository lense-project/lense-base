package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph.GraphNode

/**
 * Created by keenon on 5/21/15.
 *
 * This game player is the only one currently able to handle the asynchronous setting. It uses a very simple threshold,
 * where if the uncertainty of a node is above a certain value, we get another query for it. We assume all in flight queries
 * count against the uncertainty of a node already
 */

object ThresholdHeuristic extends GamePlayer {
  // There are a lot more of these, we must be very certain every time, or we will make mistakes
  val defaultClassThreshold = 0.985
  val otherThreshold = 0.87

  // One obsevation cuts uncertainty to the human error rate, as a rough heuristic
  val humanUncertaintyMultiple = 0.4

  override def getOptimalMove(state: GameState): GameMove = {
    // If it's been more than 10s, just force a TurnInGuess()
    if (System.currentTimeMillis() - state.startTime > 10000) {
      return TurnInGuess()
    }

    // We should probably be cacheing this if it's every used in production, but no matter
    val marginals = state.graph.marginalEstimate()
    val moveOnNodes = state.originalGraph.nodes.filter(node => {
      val dist = marginals(state.oldToNew(node))
      val maxProb = dist.maxBy(_._2)
      val inFlightRequestsForNode = state.inFlightRequests.count(_._1 eq node)
      var certainty = maxProb._2
      for (i <- 1 to inFlightRequestsForNode) {
        certainty = 1 - ((1 - certainty)*humanUncertaintyMultiple)
      }
      // println(node+": (in flight "+inFlightRequestsForNode+") - "+certainty)
      if (maxProb._1 == "O") {
        certainty < defaultClassThreshold
      }
      else {
        certainty < otherThreshold
      }
    }).toSet

    if (moveOnNodes.size > 0) {
      val desiredLegalObservations = getAllLegalMoves(state).filter{
        case MakeHumanObservation(node : GraphNode, hcu) =>
          moveOnNodes.contains(node)
        case _ => false
      }

      // If we have requests we want to make, and no annotators to perform them, just hang
      // THIS IS ARGUABLY NOT IDEAL BEHAVIOR, JUST BASELINE FOR DEMOS
      if (state.hcuPool.hcuPool.size == 0) {
        Wait()
      }
      // Otherwise find the request-human pair we think can get it done the fastest, and do that
      else if (desiredLegalObservations.size > 0) {
        desiredLegalObservations.minBy{
          case MakeHumanObservation(node, hcu) => hcu.estimateRequiredTimeIncludingQueue(node)
        }
      }
      // There's more we'd like observations on, but we have no more annotators who could provide another independent
      // sample, so we quit here
      else if (state.inFlightRequests.size > 0) {
        Wait()
      }
      else {
        TurnInGuess()
      }
    }
    else {
      // If we have in flight requests, wait for them to complete
      if (state.inFlightRequests.size > 0) Wait()
      // Turn in the guess we made
      else TurnInGuess()
    }
  }
}

