package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.humancompute.{WorkUnit, HumanComputeUnit, HCUPool}
import edu.stanford.lense_base.models.{ModelVariable, Model}
import edu.stanford.lense_base.util.AsyncBudgetManager

/**
 * Created by keenon on 4/27/15.
 *
 * This is the abstract face for all the possible general game playing agents that can make decisions for us
 */

// These are the moves you can make

abstract class GameMove
case class TurnInGuess() extends GameMove
case class MakeHumanObservation(variable : ModelVariable, hcu : HumanComputeUnit) extends GameMove
case class Wait() extends GameMove
case class WaitForTime(delay : Long) extends GameMove

// This is the head of all game playing agents

abstract class GamePlayer {
  var budget : AsyncBudgetManager = null

  def getOptimalMove(state : GameState) : GameMove

  def getAllLegalMoves(state : GameState, reserveRealBudget : Boolean = true) : List[GameMove] = {
    val maxHCUCost = if (state.hcuPool.hcuPool.size > 0) state.hcuPool.hcuPool.map(_.cost).max else 0
    val minHCUCost = if (state.hcuPool.hcuPool.size > 0) state.hcuPool.hcuPool.map(_.cost).min else 0

    var canAfford = 0.0

    if (reserveRealBudget) {
      if (budget.tryReserveBudget(maxHCUCost, state)) {
        // We've successfully reserved all the money we need
        canAfford = maxHCUCost
      }
      else {
        if (budget.tryReserveBudget(minHCUCost, state)) {
          // We've successfully reserved some of the money we need
          canAfford = minHCUCost
        }
        else {
          // We're broke, can't afford another human label
          canAfford = 0.0
        }
      }
    }
    else {
      // Don't limit on cost for hypothetical explorations
      canAfford = maxHCUCost
    }

    state.model.variables.flatMap(n => {
      state.hcuPool.synchronized {
        val list = state.hcuPool.hcuPool.
          // Take only HCU's we haven't used before for this node
          filter(hcu => !(state.inFlightRequests.exists(t => t._1.eq(n) && t._2.eq(hcu)) || state.completedRequests.exists(t => t._1.eq(n) && t._2.eq(hcu)))).
          filter(_.cost <= canAfford)
        // If there are HCUs available for this node, pick the one with the shortest queue
        if (list.size > 0) {
          List(MakeHumanObservation(n, list.minBy(_.estimateTimeToFinishQueue)))
        }
        // If there are no HCUs available for this node, then return no moves on this node
        else List()
      }
    }).toList ++ {
      if (state.inFlightRequests.size > 0) List(Wait()) else List()
    } ++ {
      if (state.inFlightRequests.size == 0) List(TurnInGuess()) else List()
    }
  }
}



// This is the current state of the "game"

case class GameState(model : Model,
           hcuPool : HCUPool,
           lossFunction : (List[(ModelVariable, String, Double)], Double, Long) => Double,
           maxLossPerNode : Double,
           cost : Double = 0.0,
           startTime : Long = System.currentTimeMillis(),
           inFlightRequests : Set[(ModelVariable, HumanComputeUnit, WorkUnit, Long)] = Set(),
           completedRequests : Set[(ModelVariable, HumanComputeUnit, WorkUnit)] = Set()) {

  // A quick and dirty way for game players to store arbitrary extra state
  var payload : Any = null

  def loss(hypotheticalExtraDelay : Long = 0) : Double = {
    val mapAssignments = model.map

    val marginals = model.marginals
    if (marginals.size != model.variables.size) {
      System.err.println("Breakpoint")
    }

    val bestGuesses = model.marginals.toList.map(pair => {
      val mapAssignmentProb = pair._2(mapAssignments(pair._1))
      val mapAssignment = mapAssignments(pair._1)
      val node = pair._1
      (node, mapAssignment, mapAssignmentProb)
    })

    if (bestGuesses.size == 0) {
      throw new IllegalStateException("BUG: Shouldn't have a bestGuesses with size 0")
    }

    lossFunction(bestGuesses, cost, hypotheticalExtraDelay + System.currentTimeMillis() - startTime)
  }

  def copyMutableState(nextState : GameState) = {
    nextState.payload = payload
  }

  def getNextStateForInFlightRequest(variable : ModelVariable, hcu : HumanComputeUnit, workUnit : WorkUnit, launchTime : Long) : GameState = {
    val nextState : GameState = GameState(model,
      hcuPool,
      lossFunction,
      maxLossPerNode,
      cost + hcu.cost,
      startTime,
      inFlightRequests ++ Set((variable, hcu, workUnit, launchTime)),
      completedRequests)
    copyMutableState(nextState)
    nextState
  }

  def getNextStateForFailedRequest(variable : ModelVariable, hcu : HumanComputeUnit, workUnit : WorkUnit) : GameState = {
    val nextState : GameState = GameState(model,
      hcuPool,
      lossFunction,
      maxLossPerNode,
      cost,
      startTime,
      inFlightRequests.filter(quad => quad._1 != variable || quad._2 != hcu),
      completedRequests)
    copyMutableState(nextState)
    nextState
  }

  // Gets a GameState that corresponds to seeing observation "obs" on node "node"
  def getNextStateForVariableObservation(variable : ModelVariable, hcu : HumanComputeUnit, workUnit : WorkUnit, obs : String) : GameState = {
    val nextState : GameState = GameState(model.cloneModelWithHumanObservation(variable, obs, hcu, workUnit.timeSinceCreation()),
      hcuPool,
      lossFunction,
      maxLossPerNode,
      cost,
      startTime,
      inFlightRequests.filter(quad => quad._1 != variable || quad._2 != hcu),
      completedRequests ++ Set((variable, hcu, workUnit)))
    copyMutableState(nextState)
    nextState
  }
}
