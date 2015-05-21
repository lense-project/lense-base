package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph.GraphNode

/**
 * Created by keenon on 5/21/15.
 *
 * Expected utility game player as described in the paper
 */
class ExpectedUtilityGamePlayer extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    val moves = getAllLegalMoves(state)
    val nodes = moves.filter(_.isInstanceOf[MakeHumanObservation]).map{
      case MakeHumanObservation(node, hcu) => node
    }
    val bestExpectedUtilityObservation = nodes.map(n => (n,getExpectedUtility(state, n))).maxBy(_._2)
    val turnInNowLoss = state.loss()
    if (turnInNowLoss < bestExpectedUtilityObservation._2) {
      TurnInGuess()
    }
    else {
      MakeHumanObservation(bestExpectedUtilityObservation._1,
        // Pick the fastest available human to make our observation with
        state.hcuPool.hcuPool.minBy(_.estimateRequiredTimeIncludingQueue(bestExpectedUtilityObservation._1))
      )
    }
  }

  def getExpectedUtility(state : GameState, node : GraphNode) : Double = {

    // Compute p_{\theta}(y | x)
    val marginals = state.graph.marginalEstimate()

    // Set p_{\theta}(\tau) = p_{\theta}(\tau | y)*p_{\theta}(y | x)
    val humanPredictionMarginals = marginals.map(pair => {
      val node = pair._1
      val distributionOverAssignments : Map[String,Double] = pair._2
      // We assume the \epsilon-random error model
      val distributionOverHumanGuesses = distributionOverAssignments.map(assignment => {
        val value = assignment._1
        val prob = assignment._2
        // TODO: multiply distribution by epsilon-random...
        (value, prob)
      })

      (node, distributionOverAssignments)
    })

    // TODO: rest
    0.0
  }
}
