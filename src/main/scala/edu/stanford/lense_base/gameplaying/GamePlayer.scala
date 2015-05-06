package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph._
import edu.stanford.lense_base.humancompute.{WorkUnit, HumanComputeUnit, HCUPool}

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
  def getOptimalMove(state : GameState) : GameMove

  def getAllLegalMoves(state : GameState) : List[GameMove] = {
    List(TurnInGuess()) ++ state.originalGraph.nodes.filter(_.observedValue == null).flatMap(n => {
      state.hcuPool.synchronized {
        state.hcuPool.hcuPool.
          // Take only HCU's we haven't used before for this node
          filter(hcu => !(state.inFlightRequests.exists(t => t._1.eq(n) && t._2.eq(hcu)) || state.completedRequests.exists(t => t._1.eq(n) && t._2.eq(hcu)))).
          // And make an observation on them
          map(hcu => MakeHumanObservation(state.oldToNew(n), hcu))
      }
    }).toList
  }
}

// This is the dumb one question per node baseline, just to make sure that everything is working as expected

object OneQuestionBaseline extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    if (state.payload == null) state.payload = (0, state.graph.nodes.size)
    val i = state.payload.asInstanceOf[(Int,Int)]
    if (i._1 < i._2) {
      state.payload = (i._1+1,i._2)
      MakeHumanObservation(state.oldToNew(state.originalGraph.nodes(i._1)), state.hcuPool.hcuPool.minBy(hcu => hcu.estimateTimeToFinishQueue))
    }
    else {
      if (state.inFlightRequests.size == 0) {
        TurnInGuess()
      }
      else {
        Wait()
      }
    }
  }
}

// This game player calculates loss if we turn in the solution now, and expected loss if we turn in after each possible query we
// could make, and takes the best possible

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
          nextStates.map(p => p._1 * p._2.loss()).sum
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
  lazy val marginals : Map[GraphNode, Map[String, Double]] = graph.marginalEstimate()

  def loss(hypotheticalExtraDelay : Long = 0) : Double = {
    val bestGuesses = marginals.toList.map(pair => {
      val maxPair = pair._2.maxBy(_._2)
      (pair._1, maxPair._1, maxPair._2)
    })
    lossFunction(bestGuesses, cost, hypotheticalExtraDelay + System.currentTimeMillis() - startTime)
  }

  def copyMutableState(nextState : GameState, oldToNew : Map[GraphNode,GraphNode]) = {
    nextState.payload = payload
    nextState.originalGraph = originalGraph
    nextState.oldToNew = oldToNew.map(pair => (pair._1, oldToNew(pair._2)))
  }

  def getNextStateForInFlightRequest(node : GraphNode, hcu : HumanComputeUnit, workUnit : WorkUnit) : GameState = {
    val clonePair = graph.clone()
    val nextState : GameState = GameState(clonePair._1,
      cost + hcu.cost,
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
      cost + hcu.cost,
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
    nextState
  }

  def getNextStates(move : GameMove) : List[(Double,GameState)] = {
    // could perform some cacheing here, to help prevent the need for explicit Dynamic Programming elsewhere
    move match {
      case obs : MakeHumanObservation => {
        obs.node.nodeType.possibleValues.map(randomHumanResponse => {
          (marginals(obs.node)(randomHumanResponse),getNextStateForNodeObservation(obs.node, obs.hcu, null, randomHumanResponse))
        })
      }.toList
      case _ : TurnInGuess => List()
    }
  }
}
