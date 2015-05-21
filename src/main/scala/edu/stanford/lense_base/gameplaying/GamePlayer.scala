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

// This is the head of all game playing agents

abstract class GamePlayer {

  var engine : LenseEngine = null

  def getOptimalMove(state : GameState) : GameMove

  def getAllLegalMoves(state : GameState) : List[GameMove] = {
    val maxHCUCost = if (state.hcuPool.hcuPool.size > 0) state.hcuPool.hcuPool.map(_.cost).max else 0
    val minHCUCost = if (state.hcuPool.hcuPool.size > 0) state.hcuPool.hcuPool.map(_.cost).min else 0

    var canAfford = 0.0

    if (engine.tryReserveBudget(maxHCUCost, this)) {
      // We've successfully reserved all the money we need
      canAfford = maxHCUCost
    }
    else {
      if (engine.tryReserveBudget(minHCUCost, this)) {
        // We've successfully reserved some of the money we need
        canAfford = minHCUCost
      }
      else {
        // We're broke, can't afford another human label
        canAfford = 0.0
      }
    }

    List(TurnInGuess()) ++ state.originalGraph.nodes.filter(_.observedValue == null).flatMap(n => {
      state.hcuPool.synchronized {
        state.hcuPool.hcuPool.
          // Take only HCU's we haven't used before for this node
          filter(hcu => !(state.inFlightRequests.exists(t => t._1.eq(n) && t._2.eq(hcu)) || state.completedRequests.exists(t => t._1.eq(n) && t._2.eq(hcu)))).
          filter(_.cost <= canAfford).
          // And make an observation on them
          map(hcu => MakeHumanObservation(n, hcu))
      }
    }).toList
  }
}



// This is the current state of the "game"

case class GameState(graph : Graph,
                     cost : Double,
                     hcuPool : HCUPool,
                     addHumanObservation : (Graph, GraphNode, String) => Unit,
                     lossFunction : (List[(GraphNode, String, Double)], Double, Long) => Double,
                     inFlightRequests : Set[(GraphNode, HumanComputeUnit, WorkUnit)] = Set(),
                     completedRequests : Set[(GraphNode, HumanComputeUnit, WorkUnit)] = Set(),
                     startTime : Long = System.currentTimeMillis()) {
  // A quick and dirty way for game players to store arbitrary extra state
  var payload : Any = null
  var originalGraph : Graph = graph
  var oldToNew : Map[GraphNode,GraphNode] = graph.nodes.map(n => (n,n)).toMap

  // On creation, we have to do full *inference*. This is very expensive, compared to everything else we do
  lazy val marginals : Map[GraphNode, Map[String, Double]] = {
    graph.marginalEstimate()
  }

  def loss(hypotheticalExtraDelay : Long = 0) : Double = {
    val bestGuesses = marginals.toList.map(pair => {
      val maxPair = pair._2.maxBy(_._2)
      (pair._1, maxPair._1, maxPair._2)
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
      inFlightRequests ++ Set((node, hcu, workUnit)),
      completedRequests,
      startTime)
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
      inFlightRequests -- Set((node, hcu, workUnit)),
      completedRequests,
      startTime)
    copyMutableState(nextState, clonePair._2)
    nextState
  }

  // Gets a GameState that corresponds to seeing observation "obs" on node "node"
  def getNextStateForNodeObservation(node : GraphNode, hcu : HumanComputeUnit, workUnit : WorkUnit, obs : String) : GameState = {
    val clonePair = graph.clone()
    val nextState : GameState = GameState(clonePair._1,
      cost + hcu.cost,
      hcuPool,
      addHumanObservation,
      lossFunction,
      inFlightRequests -- Set((node, hcu, workUnit)),
      completedRequests ++ Set((node, hcu, workUnit)),
      startTime)
    copyMutableState(nextState, clonePair._2)
    addHumanObservation(clonePair._1, nextState.oldToNew(node), obs)
    nextState
  }

  def getNextStates(move : GameMove) : List[(Double,GameState)] = {
    // could perform some cacheing here, to help prevent the need for explicit Dynamic Programming elsewhere
    move match {
      case obs : MakeHumanObservation => {
        obs.node.nodeType.possibleValues.map(randomHumanResponse => {
          (marginals(oldToNew(obs.node))(randomHumanResponse),getNextStateForNodeObservation(obs.node, obs.hcu, null, randomHumanResponse))
        })
      }.toList
      case _ : TurnInGuess => List()
    }
  }
}
