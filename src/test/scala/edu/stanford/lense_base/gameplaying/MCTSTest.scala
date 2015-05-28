package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph.{Graph, GraphNode, GraphStream}
import edu.stanford.lense_base.{ArtificialHCUPool, LenseEngine, ClippedGaussianHumanDelayDistribution, EpsilonRandomErrorDistribution}

import scala.util.Random

/**
 * Created by keenon on 5/25/15.
 *
 * Runs dummy tests against MCTS, to verify everything is working roughly as expected, before we unleash on the world
 *
 * Takes a surprising amount of machinery to simulate everything. Could maybe be designed better... no matter, reserach
 */
object MCTSTest extends App {
  val rand = new Random()
  val humanErrorDistribution = EpsilonRandomErrorDistribution(0.1, rand)
  val humanDelayDistribution = ClippedGaussianHumanDelayDistribution(3000, 1000, rand)

  val graphStream = new GraphStream()
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

  // This is to help MCTS, by both clipping search and normalizing reward functions
  val maxLossPerNode = 1.5

  def lossFunction(mostLikelyGuesses: List[(GraphNode, String, Double)], cost: Double, time: Long): Double = {
    val expectedErrors = mostLikelyGuesses.map{
      case (_, _, p) => 1.0 - p
    }.sum

    // We expect each extra cent to generate at-least a K% reduction in average error
    val K = 2.0
    expectedErrors + cost*K
  }

  val engine = new LenseEngine(graphStream, MCTSGamePlayer, humanErrorDistribution, humanDelayDistribution)
  engine.addBudget(100)

  val graph = graphStream.newGraph()
  val node1 = graph.makeNode(nodeType, toString = "node1")
  val node2 = graph.makeNode(nodeType, toString = "node2")
  val factor = graph.makeFactor(factorType, List(node1, node2))

  println(graph.marginalEstimate())

  val hcuPool = ArtificialHCUPool(10, humanErrorDistribution, humanDelayDistribution, 0.01, rand)

  var gameState = GameState(engine, graph, 0.0, hcuPool, engine.attachHumanObservation, lossFunction, 1.0)

  val optimalMove = MCTSGamePlayer.getOptimalMove(gameState)

  println("Got optimal move: "+optimalMove)
}
