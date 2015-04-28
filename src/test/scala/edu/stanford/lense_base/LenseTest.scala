package edu.stanford.lense_base

import edu.stanford.lense_base.gameplaying.{MakeHumanObservation, GameState, OneQuestionBaseline}
import edu.stanford.lense_base.graph.{GraphNode, NodeType, GraphStream}

/**
 * Created by keenon on 4/27/15.
 */
object LenseTest extends App {
  val graphStream : GraphStream = new GraphStream()
  val t : NodeType = graphStream.makeNodeType(Set("true", "false"))

  val g = graphStream.newGraph()
  val n = g.makeNode(t)

  def askHuman(n : GraphNode) : String = "true"
  def lossFunction(maxGuesses : List[(GraphNode, String, Double)], cost : Double, delay : Double) : Double = 1.0

  val lense = new Lense(graphStream, OneQuestionBaseline)

  val firstGameState = GameState(g, 0.0, 0.0, askHuman, lense.attachHumanObservation, lossFunction)

  println(firstGameState.graph.marginalEstimate())
  val secondGameState = firstGameState.takeRealMove(MakeHumanObservation(n))
  println(secondGameState.graph)
  println(secondGameState.graph.marginalEstimate())
}
