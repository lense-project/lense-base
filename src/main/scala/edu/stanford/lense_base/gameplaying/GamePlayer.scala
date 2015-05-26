package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.LenseEngine
import edu.stanford.lense_base.graph._
import edu.stanford.lense_base.humancompute.{WorkUnit, HumanComputeUnit, HCUPool}

import scala.collection.mutable
import scala.concurrent.{Await, Promise, Future}
import scala.concurrent.duration._
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by keenon on 4/27/15.
 *
 * This is the abstract face for all the possible general game playing agents that can make decisions for us
 */

// These are the moves you can make

abstract class GameMove
case class TurnInGuess() extends GameMove
case class MakeHumanObservation(node : GraphNode, hcu : HumanComputeUnit) extends GameMove
case class Wait() extends GameMove
case class WaitForTime(delay : Long) extends GameMove

// This is the head of all game playing agents

abstract class GamePlayer {

  var engine : LenseEngine = null

  def getOptimalMove(state : GameState) : GameMove

  def getAllLegalMoves(state : GameState, reserveRealBudget : Boolean = true) : List[GameMove] = {
    val maxHCUCost = if (state.hcuPool.hcuPool.size > 0) state.hcuPool.hcuPool.map(_.cost).max else 0
    val minHCUCost = if (state.hcuPool.hcuPool.size > 0) state.hcuPool.hcuPool.map(_.cost).min else 0

    var canAfford = 0.0

    if (reserveRealBudget) {
      if (engine.tryReserveBudget(maxHCUCost, state, state.hcuPool)) {
        // We've successfully reserved all the money we need
        canAfford = maxHCUCost
      }
      else {
        if (engine.tryReserveBudget(minHCUCost, state, state.hcuPool)) {
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

    state.originalGraph.nodes.filter(_.observedValue == null).flatMap(n => {
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

case class GameState(graph : Graph,
                     cost : Double,
                     hcuPool : HCUPool,
                     addHumanObservation : (Graph, GraphNode, String) => Unit,
                     lossFunction : (List[(GraphNode, String, Double)], Double, Long) => Double,
                     maxLossPerNode : Double,
                     inFlightRequests : Set[(GraphNode, HumanComputeUnit, WorkUnit)] = Set(),
                     completedRequests : Set[(GraphNode, HumanComputeUnit, WorkUnit)] = Set(),
                     startTime : Long = System.currentTimeMillis(),
                     marginalMemos : mutable.Map[Map[GraphNode,Map[String, Int]], Map[GraphNode, Map[String,Double]]] = mutable.Map(),
                     mapMemos : mutable.Map[Map[GraphNode,Map[String, Int]], Map[GraphNode, String]] = mutable.Map(),
                     observedNodes : Map[GraphNode,Map[String, Int]] = Map()) {
  // A quick and dirty way for game players to store arbitrary extra state
  var payload : Any = null
  var originalGraph : Graph = graph
  var oldToNew : Map[GraphNode,GraphNode] = graph.nodes.map(n => (n,n)).toMap

  // On creation, we have to do full *inference*. This is very expensive, compared to everything else we do
  // We do some cacheing, but this shouldn't help very often, since the search space is very large.
  lazy val marginals : Map[GraphNode, Map[String, Double]] = {
    if (!marginalMemos.contains(observedNodes)) {
      marginalMemos.put(observedNodes, graph.marginalEstimate().map(pair => {
        (oldToNew.filter(_._2 eq pair._1).head._1, pair._2)
      }))
    }
    marginalMemos(observedNodes)
  }

  lazy val map : Map[GraphNode, String] = {
    if (!mapMemos.contains(observedNodes)) {
      mapMemos.put(observedNodes, graph.mapEstimate().map(pair => {
        (oldToNew.filter(_._2 eq pair._1).head._1, pair._2)
      }))
    }
    mapMemos(observedNodes)
  }

  def loss(hypotheticalExtraDelay : Long = 0) : Double = {
    val mapAssignments = map

    val bestGuesses = marginals.toList.map(pair => {
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

  def copyMutableState(nextState : GameState, copyOldToNew : Map[GraphNode,GraphNode]) = {
    nextState.payload = payload
    nextState.originalGraph = originalGraph
    nextState.oldToNew = oldToNew.map(pair => (pair._1, copyOldToNew(pair._2)))
  }

  def getNextStateForInFlightRequest(node : GraphNode, hcu : HumanComputeUnit, workUnit : WorkUnit) : GameState = {
    val clonePair = graph.clone()
    val nextState : GameState = GameState(clonePair._1,
      cost,
      hcuPool,
      addHumanObservation,
      lossFunction,
      maxLossPerNode,
      inFlightRequests ++ Set((node, hcu, workUnit)),
      completedRequests,
      startTime,
      marginalMemos,
      mapMemos,
      observedNodes)
    copyMutableState(nextState, clonePair._2)
    nextState
  }

  def getNextStateForFailedRequest(node : GraphNode, hcu : HumanComputeUnit, workUnit : WorkUnit) : GameState = {
    val clonePair = graph.clone()
    val nextState : GameState = GameState(clonePair._1,
      cost,
      hcuPool,
      addHumanObservation,
      lossFunction,
      maxLossPerNode,
      inFlightRequests -- Set((node, hcu, workUnit)),
      completedRequests,
      startTime,
      marginalMemos,
      mapMemos,
      observedNodes)
    copyMutableState(nextState, clonePair._2)
    nextState
  }

  // Gets a GameState that corresponds to seeing observation "obs" on node "node"
  def getNextStateForNodeObservation(node : GraphNode, hcu : HumanComputeUnit, workUnit : WorkUnit, obs : String) : GameState = {
    val clonePair = graph.clone()

    // Figure out the observed nodes set for the next state
    val nextObservedNodes : Map[GraphNode, Map[String,Int]] = if (observedNodes.map(_._1).toSet.contains(node)) {
      observedNodes.map(pair => {
        if (pair._1 eq node) {
          if (pair._2.map(_._1).toSet.contains(obs)) {
            (pair._1, pair._2.map(obsCount => {
              if (obsCount._1 == obs) {
                (obsCount._1, obsCount._2 + 1)
              }
              else obsCount
            }))
          }
          else (pair._1, pair._2 ++ Map(obs -> 1))
        }
        else {
          pair
        }
      })
    } else {
      observedNodes ++ Map(node -> Map(obs -> 1))
    }

    val nextState : GameState = GameState(clonePair._1,
      cost + hcu.cost,
      hcuPool,
      addHumanObservation,
      lossFunction,
      maxLossPerNode,
      inFlightRequests -- Set((node, hcu, workUnit)),
      completedRequests ++ Set((node, hcu, workUnit)),
      startTime,
      marginalMemos,
      mapMemos,
      nextObservedNodes)
    copyMutableState(nextState, clonePair._2)
    addHumanObservation(clonePair._1, nextState.oldToNew(node), obs)
    nextState
  }
}
