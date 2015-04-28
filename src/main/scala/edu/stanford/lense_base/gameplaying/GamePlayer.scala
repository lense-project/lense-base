package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph._

/**
 * Created by keenon on 4/27/15.
 *
 * This is the abstract face for all the possible general game playing agents that can make decisions for us
 */

// These are the moves you can make

abstract class GameMove
case class TurnInGuess() extends GameMove
case class MakeHumanObservation(node : GraphNode) extends GameMove

// This is the head of all game playing agents

abstract class GamePlayer {
  def getOptimalMove(state : GameState) : GameMove

  def getAllLegalMoves(state : GameState) : List[GameMove] = {
    List(TurnInGuess()) ++ state.originalGraph.nodes.filter(_.observedValue == null).map(n => MakeHumanObservation(state.oldToNew(n))).toList
  }
}

// This is the dumb one question per node baseline, just to make sure that everything is working as expected

object OneQuestionBaseline extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    if (state.payload == null) state.payload = (0, state.graph.nodes.size)
    val i = state.payload.asInstanceOf[(Int,Int)]
    if (i._1 < i._2) {
      state.payload = (i._1+1,i._2)
      MakeHumanObservation(state.oldToNew(state.originalGraph.nodes(i._1)))
    }
    else TurnInGuess()
  }
}

// This game player calculates loss if we turn in the solution now, and expected loss if we turn in after each possible query we
// could make, and takes the best possible

object LookaheadOneHeuristic extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    getAllLegalMoves(state).map(m => {
      val nextStates = state.getNextStates(m)
      // get loss now
      val moveLoss =
        if (nextStates.size == 0) state.loss
        else {
          // get expected loss at next state
          nextStates.map(p => p._1 * p._2.loss).sum
        }
      (moveLoss, m)
    }).maxBy(_._1)._2
  }
}

// This is the current state of the "game"

case class GameState(graph : Graph,
                     cost : Double,
                     delay : Double,
                     getHumanObservation : (GraphNode) => String,
                     addHumanObservation : (Graph, GraphNode, String) => Unit,
                     lossFunction : (List[(GraphNode, String, Double)], Double, Double) => Double) {
  // A quick and dirty way for game players to store arbitrary extra state
  var payload : Any = null
  var originalGraph : Graph = graph
  var oldToNew : Map[GraphNode,GraphNode] = graph.nodes.map(n => (n,n)).toMap

  val assumedHumanCost = 1.0
  val assumedHumanDelay = 1.0

  // On creation, we have to do full *inference*. This is very expensive, compared to everything else we do
  lazy val marginals : Map[GraphNode, Map[String, Double]] = graph.marginalEstimate()
  lazy val loss : Double = {
    val bestGuesses = marginals.toList.map(pair => {
      val maxPair = pair._2.maxBy(_._2)
      (pair._1, maxPair._1, maxPair._2)
    })
    lossFunction(bestGuesses, cost, delay)
  }

  // Gets a GameState that corresponds to seeing observation "obs" on node "node"
  private def getNextStateForNodeObservation(node : GraphNode, obs : String) : GameState = {
    val clonePair = graph.clone()
    val nextState : GameState = GameState(clonePair._1, cost + assumedHumanCost, delay + assumedHumanDelay, getHumanObservation, addHumanObservation, lossFunction)
    nextState.payload = payload
    nextState.originalGraph = originalGraph
    nextState.oldToNew = oldToNew.map(pair => (pair._1, clonePair._2(pair._2)))
    addHumanObservation(nextState.graph, clonePair._2(node), obs)
    nextState
  }

  def takeRealMove(obs : MakeHumanObservation) : GameState = {
    getNextStateForNodeObservation(obs.node, getHumanObservation(obs.node))
  }

  def getNextStates(move : GameMove) : List[(Double,GameState)] = {
    // could perform some cacheing here, to help prevent the need for explicit Dynamic Programming elsewhere
    move match {
      case obs : MakeHumanObservation => {
        obs.node.nodeType.possibleValues.map(randomHumanResponse => {
          (marginals(obs.node)(randomHumanResponse),getNextStateForNodeObservation(obs.node, randomHumanResponse))
        })
      }.toList
      case _ : TurnInGuess => List()
    }
  }
}
