package edu.stanford.nlp.chinese_restaurant

import edu.stanford.lense_base.LenseMulticlassUseCase
import edu.stanford.lense_base.gameplaying.{ThresholdHeuristic, OneQuestionBaseline, GamePlayer}
import edu.stanford.lense_base.graph.GraphNode

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

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
}
