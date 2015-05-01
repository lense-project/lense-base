package edu.stanford.lense_base

import edu.stanford.lense_base.gameplaying.{MakeHumanObservation, GameState, OneQuestionBaseline}
import edu.stanford.lense_base.graph.{Graph, GraphNode, NodeType, GraphStream}

import scala.concurrent.{Promise, Future}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

/**
 * Created by keenon on 4/27/15.
 */
object LenseTest extends App {
  val graphStream : GraphStream = new GraphStream()
  val t : NodeType = graphStream.makeNodeType(Set("true", "false"))

  val g = graphStream.newGraph()
  val n = g.makeNode(t)

  def askHuman(n : GraphNode) : Promise[String] = {
    val p = Promise[String]()
    p.complete(Try { "true" })
    p
  }
  def lossFunction(maxGuesses : List[(GraphNode, String, Double)], cost : Double, delay : Double) : Double = 1.0

  val lense = new Lense(graphStream, OneQuestionBaseline)

  val firstGameState = GameState(g, 0.0, 0.0, askHuman, lense.attachHumanObservation, lossFunction)

  println(firstGameState.graph.marginalEstimate())
  val secondGameState = firstGameState.takeRealMove(MakeHumanObservation(n))
  println(secondGameState.graph)
  println(secondGameState.graph.marginalEstimate())
  val thirdGameState = secondGameState.takeRealMove(MakeHumanObservation(secondGameState.graph.nodes(0)))
  println(thirdGameState.graph)
  println(thirdGameState.graph.marginalEstimate())

  firstGameState.graph.nodes(0).observedValue = "true"

  println("learning")
  println("pre-learning weights")
  println(graphStream.withDomainList)
  println("peforming GD")
  lense.pastGuesses += firstGameState.graph
  lense.learnHoldingPastGuessesConstant()
  println("post-learning weights")
  println(graphStream.withDomainList)

  firstGameState.graph.nodes(0).observedValue = null

  println(firstGameState.graph.marginalEstimate())
  println(secondGameState.graph.factors.size)
  println(secondGameState.graph.factors)
  println(secondGameState.graph.marginalEstimate())
  println(thirdGameState.graph.marginalEstimate())
}
