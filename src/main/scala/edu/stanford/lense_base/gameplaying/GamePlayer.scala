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

// This is the dumb one question per node baseline, just to make sure that everything is working as expected

class NQuestionBaseline(n : Int) extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
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

// This game player is the only one currently able to handle the asynchronous setting. It uses a very simple threshold,
// where if the uncertainty of a node is above a certain value, we get another query for it. We assume all in flight queries
// count against the uncertainty of a node already.

object ThresholdHeuristic extends GamePlayer {
  val threshold = 0.9
  // One obsevation cuts uncertainty to the human error rate, as a rough heuristic
  val humanUncertaintyMultiple = 0.3

  override def getOptimalMove(state: GameState): GameMove = {
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
      certainty < threshold
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
