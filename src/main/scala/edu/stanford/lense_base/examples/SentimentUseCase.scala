package edu.stanford.lense_base.examples

import edu.stanford.lense_base.gameplaying.{ThresholdHeuristic, GamePlayer}
import edu.stanford.lense_base.humancompute.{EpsilonRandomErrorDistribution, ClippedGaussianHumanDelayDistribution}
import edu.stanford.lense_base.LenseMulticlassUseCase
import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.models.{UnivariateExternalModelStream, LogisticExternalModelStream, ModelStream, ModelVariable}
import edu.stanford.nlp.word2vec.Word2VecLoader

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
 * Created by keenon on 5/20/15.
 *
 * Using the Stanford sentiment dataset for a simple turk-able question set
 */
class SentimentUseCase extends LenseMulticlassUseCase[String] {
  lazy val trainSet : List[(String,String)] = loadData("data/sentiment/aclImdb/train").take(20)
  lazy val fullTestSet : List[(String,String)] = loadData("data/sentiment/aclImdb/test")
  lazy val testSet : List[(String,String)] = fullTestSet.take(1000)
  lazy val devSet : List[(String,String)] = fullTestSet.filter(!testSet.contains(_)).take(400)

  lazy val word2vec : java.util.Map[String, Array[Double]] = try {
    // Word2VecLoader.loadData("data/google-300.ser.gz")
    new java.util.HashMap[String, Array[Double]]()
  } catch {
    case e : Throwable =>
      // Couldn't load word vectors
      System.err.println("*** COULDN'T LOAD WORD VECTORS")
      e.printStackTrace()
      // return an empty map
      new java.util.HashMap[String, Array[Double]]()
  }

  override def labelTypes: List[String] = List("POS", "NEG")

  override def getHumanVersionOfLabel(label: String): String = label match {
    case "POS" => "Positive"
    case "NEG" => "Negative"
    case a => a
  }

  // The null class for during analysis
  override def defaultClass : String = "NEG"

  override def initialTrainingData : List[(String, String)] = trainSet

  lazy val logisticModelStream : ModelStream = new LogisticExternalModelStream[String](humanErrorDistribution, labelTypes) {
    override def getFeatures(input: String): Map[String, Double] = {
      // Stupid bag of words features...
      val tokens = input.split(" ")
      val unigrams = tokens.map(tok => (tok, 1.0)).toMap
      val bigrams = (0 to tokens.size - 2).map(i => (tokens(i) + " "+ tokens(i+1), 1.0)).toMap
      val basicFeatures = unigrams ++ bigrams

      // Also normalized word embedding for whole document
      val embedding = new Array[Double](300)
      for (tok <- input.split(" ")) {
        word2vec.get(tok) match {
          case vec : Array[Double] =>
            (0 to vec.length-1).foreach(i => embedding(i) += vec(i))
          case null =>
        }
      }

      val squareSum = embedding.map(x => x*x).sum
      val normalized = embedding.map(_ / Math.sqrt(squareSum))

      if (squareSum > 0) {
        (0 to normalized.length-1).map(i => "e" + i -> normalized(i)).toMap // ++ basicFeatures
      }
      else {
        basicFeatures
      }
    }

    /**
     * Defines the possible output values of the model
     * @return
     */
    override def possibleValues: List[String] = labelTypes.toList
  }
  override def getModelStream: ModelStream = logisticModelStream

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
  override def lossFunction(mostLikelyGuesses: List[(ModelVariable, String, Double)], cost: Double, ms: Long): Double = {
    val uncertainty = 1 - mostLikelyGuesses(0)._3
    uncertainty*10 + cost
  }

  override val maxLossPerNode : Double = {
    1.0
  }

  override def useCaseReportSubpath : String = "sentiment"

  /**
   * This specifies the budget that this run will spend, in dollars. You may not use all of it, but the engine will stop
   * asking humans for help, and revert to simple machine learning, after it has exhausted the budget.
   *
   * @return amount in dollars to use as budget
   */
  override def budget: Double = 100.0

  override def gamePlayer : GamePlayer = ThresholdHeuristic

  lazy val random = new Random()
  lazy val humanErrorDistribution = EpsilonRandomErrorDistribution(0.3, random)
  lazy val humanDelayDistribution = ClippedGaussianHumanDelayDistribution(2000, 500, random)

  // Reads positive and negative reviews in equal amounts from the given path, up to limitSize,
  // and shuffles the order of the results
  def loadData(path : String, limitSize : Int = 1000) : List[(String, String)] = {
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
  // sentimentUseCase.testWithArtificialHumans(sentimentUseCase.testSet, sentimentUseCase.devSet, sentimentUseCase.humanErrorDistribution, sentimentUseCase.humanDelayDistribution, 0.01, poolSize, "artificial_human")
  // sentimentUseCase.testBaselineForAllHuman(sentimentUseCase.testSet, sentimentUseCase.devSet, sentimentUseCase.humanErrorDistribution, sentimentUseCase.humanDelayDistribution, 0.01, poolSize, 1) // 1 query baseline
  // sentimentUseCase.testBaselineForAllHuman(sentimentUseCase.testSet, sentimentUseCase.devSet, sentimentUseCase.humanErrorDistribution, sentimentUseCase.humanDelayDistribution, 0.01, poolSize, 3) // 3 query baseline
  sentimentUseCase.testBaselineForOfflineLabeling(sentimentUseCase.testSet, sentimentUseCase.devSet)
  // sentimentUseCase.testWithRealHumans(sentimentUseCase.testSet, sentimentUseCase.devSet, poolSize)
}