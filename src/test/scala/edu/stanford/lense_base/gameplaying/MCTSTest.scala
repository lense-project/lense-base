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
  val humanErrorDistribution = EpsilonRandomErrorDistribution(0.3, rand)
  val humanDelayDistribution = ClippedGaussianHumanDelayDistribution(3000, 1000, rand)

  val graphStream = new GraphStream()
  val nodeType = graphStream.makeNodeType(Set("RED","GREEN"), Map(
    "RED" -> Map("BIAS" -> 1.0),
    "GREEN" -> Map("BIAS" -> -1.0)
  ))
  val factorType = graphStream.makeFactorType(List(nodeType, nodeType), Map(
    // Initialize with feature weights on the BIAS term, so the marginalizer has something to do
    List("RED","GREEN") -> Map("BIAS" -> 1.0),
    List("GREEN","RED") -> Map("BIAS" -> -1.0)
  ))

  def lossFunction(mostLikelyGuesses: List[(GraphNode, String, Double)], cost: Double, time: Long): Double = {
    val expectedErrors = mostLikelyGuesses.map{
      // we much prefer to not tag 0s incorrectly
      case (_,"0",p) => (1.0 - p)*50.0
      case t => 1.0 - t._3
    }.sum
    expectedErrors + cost*5
  }

  val engine = new LenseEngine(graphStream, MCTSGamePlayer, humanErrorDistribution, humanDelayDistribution)
  engine.addBudget(100)

  val graph = graphStream.newGraph()
  val node1 = graph.makeNode(nodeType)
  val node2 = graph.makeNode(nodeType)
  val factor = graph.makeFactor(factorType, List(node1, node2))

  val hcuPool = ArtificialHCUPool(10, humanErrorDistribution, humanDelayDistribution, 0.01, rand)

  var gameState = GameState(graph, 0.0, hcuPool, engine.attachHumanObservation, lossFunction)

  val optimalMove = MCTSGamePlayer.getOptimalMove(gameState)

  println("Got optimal move: "+optimalMove)
}
