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

  def getAllLegalMoves(graph : Graph) : List[GameMove] = {
    List(TurnInGuess()) ++ graph.nodes.filter(_.observedValue != null).map(n => MakeHumanObservation(n)).toList
  }
}

// This is the dumb one question per node baseline, just to make sure that everything is working as expected

object OneQuestionBaseline extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    println(state.payload)
    if (state.payload == null) state.payload = (0, state.graph.nodes.size)
    val i = state.payload.asInstanceOf[(Int,Int)]
    if (i._1 < i._2) {
      state.payload = (i._1+1,i._2)
      MakeHumanObservation(state.graph.nodes(i._1))
    }
    else TurnInGuess()
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

  def takeRealMove(obs : MakeHumanObservation) : GameState = {
    val nextState : GameState = GameState(graph.clone(), cost + assumedHumanCost, delay + assumedHumanDelay, getHumanObservation, addHumanObservation, lossFunction)
    nextState.payload = payload
    addHumanObservation(nextState.graph, obs.node, getHumanObservation(obs.node))
    nextState
  }

  def getNextStates(move : GameMove) : List[(Double,GameState)] = {
    // could perform some cacheing here, to help prevent the need for explicit Dynamic Programming elsewhere
    move match {
      case obs : MakeHumanObservation => {
        obs.node.nodeType.possibleValues.map(randomHumanResponse => {
          val nextState : GameState = GameState(graph.clone(), cost + assumedHumanCost, delay + assumedHumanDelay, getHumanObservation, addHumanObservation, lossFunction)
          nextState.payload = payload
          addHumanObservation(nextState.graph, obs.node, randomHumanResponse)
          (marginals(obs.node)(randomHumanResponse),nextState)
        })
      }.toList
      case _ : TurnInGuess => List()
    }
  }
}
