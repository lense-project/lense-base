package edu.stanford.lense_base.examples

import edu.stanford.lense_base.LenseMulticlassUseCase
import edu.stanford.lense_base.graph.GraphNode

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
 * Created by keenon on 5/20/15.
 *
 * Using the Stanford sentiment dataset for a simple turk-able question set
 */
class SentimentUseCase extends LenseMulticlassUseCase[String] {
  lazy val trainSet : List[(String,String)] = loadData("data/sentiment/aclImdb/train")
  lazy val testSet : List[(String,String)] = loadData("data/sentiment/aclImdb/test")

  override def labelTypes: Set[String] = Set("POS", "NEG")

  override def getHumanVersionOfLabel(label: String): String = label match {
    case "POS" => "Positive"
    case "NEG" => "Negative"
    case a => a
  }

  override def initialTrainingData : List[(String, String)] = trainSet

  override def getFeatures(input: String): Map[String, Double] = {
    // Stupid bag of words features... this almost certainly needs to get more sophisticated
    input.split(" ").map(tok => (tok, 1.0)).toMap
  }

  override def getHumanQuestion(input: String): String = {
    "Please label this movie review as either positive or negative: <div class='review'>"+input+"</div>"
  }

  /**
   * A way to define the loss function for you system. mostLikelyGuesses is a list of all the nodes being chosen on,
   * with their corresponding most likely label, and the probability the model assigns to the label.
   *
   * TODO: more docs here
   *
   * @param mostLikelyGuesses
   * @param cost
   * @param ms
   * @return
   */
  override def lossFunction(mostLikelyGuesses: List[(GraphNode, String, Double)], cost: Double, ms: Long): Double = {
    (1 - mostLikelyGuesses(0)._3) + cost + (ms / 1000)
  }

  override def useCaseReportSubpath : String = "sentiment"

  /**
   * This specifies the budget that this run will spend, in dollars. You may not use all of it, but the engine will stop
   * asking humans for help, and revert to simple machine learning, after it has exhausted the budget.
   *
   * @return amount in dollars to use as budget
   */
  override def budget: Double = 100.0

  // Reads positive and negative reviews in equal amounts from the given path, up to limitSize,
  // and shuffles the order of the results
  def loadData(path : String, limitSize : Int = 5) : List[(String, String)] = {
    val examples = ListBuffer[(String, String)]()

    var numNeg = 0
    for (line <- Source.fromFile(path+"/neg/neg.combined").getLines()) {
      if (!line.startsWith("==>") && line.size > 0) {
        if (numNeg < limitSize / 2) {
          examples.+=((line, "NEG"))
          numNeg += 1
        }
      }
    }

    var numPos = 0
    for (line <- Source.fromFile(path+"/pos/pos.combined").getLines()) {
      if (!line.startsWith("==>") && line.size > 0) {
        if (numPos < limitSize / 2) {
          examples.+=((line, "POS"))
          numPos += 1
        }
      }
    }
    System.err.println("Loaded "+examples.size+" examples from "+path)

    val rand = new Random(42)
    // Make sure it's not just all NEG, then all POS
    rand.shuffle(examples).toList
  }
}

object SentimentUseCase extends App {
  val sentimentUseCase = new SentimentUseCase()

  val poolSize = 10
  sentimentUseCase.testWithArtificialHumans(sentimentUseCase.testSet, 0.3, 2000, 500, 0.01, poolSize, "artificial_human")
  // sentimentUseCase.testBaselineForAllHuman(sentimentUseCase.testSet, 0.3, 2000, 500, 0.01, poolSize, 1) // 1 query baseline
  // sentimentUseCase.testBaselineForAllHuman(sentimentUseCase.testSet, 0.3, 2000, 500, 0.01, poolSize, 3) // 3 query baseline
  // sentimentUseCase.testBaselineForOfflineLabeling(sentimentUseCase.testSet)
  // sentimentUseCase.testWithRealHumans(sentimentUseCase.testSet)
}