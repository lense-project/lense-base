package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph.GraphNode

/**
 * Created by keenon on 5/28/15.
 *
 * Plays a simple game of Red-Green classification
 */
object RedGreenGameplayerHarness extends GameplayerHarness {
  // This is to help MCTS, by both clipping search and normalizing reward functions
  val maxLossPerNode = 1.5

  override def lossFunction(mostLikelyGuesses: List[(GraphNode, String, Double)], cost: Double, time: Long): Double = {
    val expectedErrors = mostLikelyGuesses.map{
      case (_, _, p) => 1.0 - p
    }.sum

    // We expect each extra cent to generate at-least a K% reduction in average error
    val K = 1.0
    expectedErrors + cost*K
  }

  def main(args: Array[String]) {
    val nodeType = graphStream.makeNodeType(Set("RED","GREEN"), Map(
      "RED" -> Map("BIAS" -> Math.log(0.9)),
      "GREEN" -> Map("BIAS" -> Math.log(0.1))
    ))

    val factorType = graphStream.makeFactorType(List(nodeType, nodeType), Map(
      // Initialize with feature weights on the BIAS term, so the marginalizer has something to do
      List("RED","RED") -> Map("BIAS" -> Math.log(0.1)),
      List("GREEN","GREEN") -> Map("BIAS" -> Math.log(0.1)),
      List("RED","GREEN") -> Map("BIAS" -> Math.log(0.4)),
      List("GREEN","RED") -> Map("BIAS" -> Math.log(0.1))
    ))

    val graph = graphStream.newGraph()
    val node1 = graph.makeNode(nodeType, toString = "node1")
    val node2 = graph.makeNode(nodeType, toString = "node2")
    val factor = graph.makeFactor(factorType, List(node1, node2))

    // Actually play out the game
    runGame(graph, MCTSGamePlayer)
  }
}
