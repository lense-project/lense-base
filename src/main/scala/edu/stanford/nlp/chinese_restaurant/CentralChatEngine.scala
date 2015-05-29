package edu.stanford.nlp.chinese_restaurant

import edu.stanford.lense_base.humancompute.{EpsilonRandomErrorDistribution, ClippedGaussianHumanDelayDistribution}
import edu.stanford.lense_base.LenseMulticlassUseCase
import edu.stanford.lense_base.gameplaying.{ThresholdHeuristic, OneQuestionBaseline, GamePlayer}
import edu.stanford.lense_base.graph.GraphNode

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
 * Created by keenon on 5/7/15.
 *
 * Manages piping data around during classification, slotfilling, etc
 */
class CentralChatEngine extends ChatEngine {
  val responseClassifier = new ResponseTypeClassification()

  override def receiveMessage(msg: String, sendReply: (String) => Unit): Unit = {
    println("Running "+msg+" against responseClassifier")
    val responseClass = Await.result(responseClassifier.getOutput(msg).future, 1000 days)
    responseClass match {
      case "snark" => sendReply("Wow, \""+msg+"\"... that's a stupid thing to say")
      case "mom" => sendReply("Your mom likes \""+msg+"\"")
      case "wow" => sendReply("OMG \""+msg+"\" very wow, such amaze")
    }
  }
}

class ResponseTypeClassification extends LenseMulticlassUseCase[String] {
  override def gamePlayer: GamePlayer = ThresholdHeuristic

  override def labelTypes: Set[String] = Set("snark", "wow", "mom")

  override def getHumanQuestion(input: String): String = {
    "How should I respond to this?</br><pre>"+input+"</pre>"
  }

  override def getFeatures(input: String): Map[String, Double] = {
    input.split(" ").map(token => ("word:"+token.toLowerCase, 1.0)).toMap ++
    Map(
      // "length-indicator:"+input.split(" ").size -> 1.0,
      // "length:" -> input.split(" ").size.toDouble,
      "BIAS" -> 0.0
    )
  }

  override def getHumanVersionOfLabel(label: String): String = label match {
    case "snark" => "Snarky response"
    case "wow" => "Doge response"
    case "mom" => "Your mom response"
    case _ => ""
  }

  /**
   * A way to define the loss function for you system. mostLikelyGuesses is a list of all the nodes being chosen on,
   * with their corresponding most likely label, and the probability the model assigns to the label.
   *
   * @param mostLikelyGuesses
   * @param cost
   * @param ms
   * @return
   */
  override def lossFunction(mostLikelyGuesses: List[(GraphNode, String, Double)], cost: Double, ms: Long): Double = {
    cost
  }

  /**
   * This specifies the budget that this run will spend, in dollars. You may not use all of it, but the engine will stop
   * asking humans for help, and revert to simple machine learning, after it has exhausted the budget.
   *
   * @return amount in dollars to use as budget
   */
  override def budget: Double = 10.0

  lazy val random = new Random(42)
  override lazy val humanErrorDistribution = EpsilonRandomErrorDistribution(0.3, random)
  override lazy val humanDelayDistribution = ClippedGaussianHumanDelayDistribution(2000, 500, random)

  /**
   * Some gameplayers care about losses being expressed as reward (negative loss) in the [0,1] range. To accomplish this,
   * we need to know a max loss per node
   * @return
   */
  override def maxLossPerNode: Double = 1.0
}
