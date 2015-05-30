package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.humancompute.{WorkUnit, HumanComputeUnit}
import edu.stanford.lense_base.models.ModelVariable

/**
 * Created by keenon on 5/21/15.
 *
 * This game player calculates loss if we turn in the solution now, and expected loss if we turn in after each possible query we
 * could make, and takes the best possible
 */

object LookaheadOneHeuristic extends GamePlayer {
  def inFlightCombinatorial(originalState : GameState, state : GameState, inFlightList : List[(ModelVariable, HumanComputeUnit, WorkUnit, Long)], i : Int, prob : Double): List[(GameState,Double)] = {
    if (inFlightList.size == 0) return List((originalState, 1.0))
    val query = inFlightList(i)

    // Terminal state
    if (i == inFlightList.size - 1) {
      query._1.possibleValues.map(value => {
        (state.getNextStateForVariableObservation(query._1, query._2, query._3, value), prob * originalState.model.marginals(query._1)(value))
      })
    }
    else {
      query._1.possibleValues.flatMap(value => {
        inFlightCombinatorial(originalState, state.getNextStateForVariableObservation(query._1, query._2, query._3, value), inFlightList, i+1, prob * originalState.model.marginals(query._1)(value))
      })
    }
  }

  override def getOptimalMove(state: GameState): GameMove = {
    println("Getting optimal move...")

    val allLegalMoves = getAllLegalMoves(state)

    val inFlightLimit = 2

    if (state.inFlightRequests.size > inFlightLimit) {
      println("Gameplayer can't handle more than "+inFlightLimit+" in flight requests simultaneously, waiting for some to return")
      Wait()
    }
    else {
      val allInFlightOutcomes = inFlightCombinatorial(state, state, state.inFlightRequests.toList, 0, 1.0)

      println("Taking expectations against " + allInFlightOutcomes.size + " outcomes of " + state.inFlightRequests.size + " in flight requests.")

      // This could be super expensive
      val expectedLossDoNothing = allInFlightOutcomes.map(pair => pair._1.loss() * pair._2).sum

      var bestGameMove: GameMove = TurnInGuess()
      var bestGameMoveLoss = expectedLossDoNothing

      println("TurnInGuess() Expected loss: "+expectedLossDoNothing)

      state.model.variables.foreach(variable => {
        val numObservationsOnThisNodeAvailable = allLegalMoves.count {
          case MakeHumanObservation(v, hcu) if v eq variable => true
          case _ => false
        }

        if (numObservationsOnThisNodeAvailable > 0) {
          val observationMove: MakeHumanObservation = allLegalMoves.filter {
            case MakeHumanObservation(v, hcu) if v eq variable => true
            case _ => false
          }.minBy {
            case MakeHumanObservation(n, hcu) => hcu.estimateTimeToFinishQueue
          }.asInstanceOf[MakeHumanObservation]

          val expectedLossForNode = variable.possibleValues.map(value => {
            allInFlightOutcomes.map(pair => {
              val thisOutcomeState = pair._1
              val thisOutcomeProb = pair._2
              thisOutcomeState.getNextStateForVariableObservation(variable, observationMove.hcu, null, value).loss() * thisOutcomeProb
            }).sum * state.model.marginals(variable)(value)
          }).sum

          println("Observe "+variable+" expected loss: "+expectedLossForNode)

          if (expectedLossForNode < bestGameMoveLoss) {
            bestGameMoveLoss = expectedLossForNode
            bestGameMove = observationMove
          }
        }
      })

      println("Best loss: "+bestGameMoveLoss+", best move: "+bestGameMove)

      if (bestGameMove == TurnInGuess() && state.inFlightRequests.size > 0) {
        Wait()
      }
      else {
        bestGameMove
      }
    }
  }
}

