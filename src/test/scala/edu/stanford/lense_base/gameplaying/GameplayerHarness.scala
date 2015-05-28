package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph.{Graph, GraphNode, GraphStream}
import edu.stanford.lense_base._
import edu.stanford.lense_base.humancompute.{HumanComputeUnit, WorkUnit}

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._
import scala.util.Random

/**
 * Created by keenon on 5/28/15.
 *
 * This is a down and dirty way to do diagnostic runs against a gameplayer and print out behavior
 */
abstract class GameplayerHarness {
  val rand = new Random()
  def humanErrorDistribution : HumanErrorDistribution = EpsilonRandomErrorDistribution(0.1, rand)
  def humanDelayDistribution : HumanDelayDistribution = ClippedGaussianHumanDelayDistribution(3000, 1000, rand)

  val graphStream = new GraphStream()

  val engine = new LenseEngine(graphStream, MCTSGamePlayer, humanErrorDistribution, humanDelayDistribution)
  engine.addBudget(100)
  val hcuPool = ArtificialHCUPool(10, humanErrorDistribution, humanDelayDistribution, 0.01, rand)

  def runGame(graph : Graph, gameplayer : GamePlayer): Unit = {
    graphStream.model.warmUpIndexes(graph)

    gameplayer.engine = engine
    engine.gamePlayer = gameplayer

    val guessMap = Await.result(engine.predict(graph, (n, hcu) => {
      val correct = ""

      val promise = Promise[String]()
      val workUnit = ArtificialWorkUnit(promise, humanErrorDistribution.guess(correct, n.nodeType.possibleValues), n, hcuPool)
      hcu.addWorkUnit(workUnit)
      workUnit
    }, hcuPool, lossFunction, maxLossPerNode).future, 1000 days)

    val predictionSummary = guessMap._2
    println("Game play logs:")
    println(predictionSummary.behaviorLog)
  }

  // This is to help MCTS, by both clipping search and normalizing reward functions
  def maxLossPerNode : Double

  def lossFunction(mostLikelyGuesses: List[(GraphNode, String, Double)], cost: Double, time: Long): Double
}
