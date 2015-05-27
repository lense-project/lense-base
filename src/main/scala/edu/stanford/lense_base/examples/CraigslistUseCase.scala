package edu.stanford.lense_base.examples

import java.io.{FileWriter, BufferedWriter, File}

import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.humancompute.HumanComputeUnit
import edu.stanford.lense_base._
import edu.stanford.nlp.word2vec.Word2VecLoader

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
 * Created by keenon on 5/3/15.
 *
 * Does Craigslist information extraction using sequence use cases
 */
class CraigslistUseCase extends LenseSequenceUseCase {
  lazy val trainSet : List[(List[String],List[String])] = loadData("data/craigslist/ads_train.txt")
  lazy val devSet : List[(List[String],List[String])] = loadData("data/craigslist/ads_dev.txt")
  lazy val testSet : List[(List[String],List[String])] = loadData("data/craigslist/ads_test.txt")

  lazy val word2vec : java.util.Map[String, Array[Double]] = try {
    Word2VecLoader.loadData("data/google-300.ser.gz")
    // new java.util.HashMap[String, Array[Double]]()
  } catch {
    case e : Throwable =>
      // Couldn't load word vectors
      System.err.println("*** COULDN'T LOAD WORD VECTORS")
      e.printStackTrace()
      // return an empty map
      new java.util.HashMap[String, Array[Double]]()
  }

  override def initialTrainingData : List[(List[String], List[String])] = trainSet

  override def labelTypes: Set[String] = (trainSet ++ devSet ++ testSet).flatMap(_._2).distinct.toSet

  override def getHumanQuestion(sequence: List[String], i: Int): String = {
    var question = "What information is the bolded word saying?<br>"
    question += "<span class='content'>"
    for (j <- 0 to sequence.length-1) {
      if (j > 0) question += " "
      if (i == j) question += "<span class='focus'> "
      question += sequence(j)
      if (i == j) question += "</span>"
    }
    question += "</span>"
    question
  }

  override def getHumanVersionOfLabel(label: String): String = label match {
    case "ADDRESS" => "Address of the Ad"
    case "RENT" => "Rent amount"
    case "LOC" => "Location"
    case "PER" => "Person"
    case "O" => "None of the above"
    case a => a
  }

  override def lossFunction(sequence: List[String], mostLikelyGuesses: List[(Int, String, Double)], cost: Double, time: Double): Double = {
    val expectedErrors = mostLikelyGuesses.map{
      // we much prefer to not tag 0s incorrectly
      case (_,"0",p) => (1.0 - p)*5.0
      case t => 1.0 - t._3
    }.sum
    // This will trade 10 human labels for fully correct token
    expectedErrors*4 + cost
  }

  override val maxLossPerNode : Double = {
    1.0
  }

  override def featureExtractor(sequence: List[String], i: Int): Map[String, Double] = {
    val basicFeatures = Map(
      "token:" + sequence(i).toLowerCase -> 1.0,
      "capitalized:" + (sequence(i)(0).isUpper && sequence(i).exists(_.isLower)) -> 1.0,
      "BIAS" -> 0.0
    )
    word2vec.get(sequence(i)) match {
      case vec : Array[Double] =>
        (0 to vec.length-1).map(i => "word2vec" + i -> vec(i)).toMap ++ basicFeatures
      case null => basicFeatures
    }
  }

  override def useCaseReportSubpath : String = "craigslist"

  def loadData(path : String) : List[(List[String],List[String])] = {
    val loadedData : ListBuffer[(List[String],List[String])] = ListBuffer()
    val currentSentence : ListBuffer[String] = ListBuffer()
    val currentSemanticSlots : ListBuffer[String] = ListBuffer()

    for (line <- Source.fromFile(path).getLines()) {
      val parts = line.split(" ")
      // This is a continuation of the old ad
      if (parts.size == 2) {
        val word: String = parts(0)
        val tag: String = parts(1)
        currentSentence.+=(word)
        currentSemanticSlots.+=(tag)
      }
      // This is a new ad
      else {
        loadedData+=((currentSentence.toList, currentSemanticSlots.toList))
        currentSentence.clear()
        currentSemanticSlots.clear()
      }
    }

    loadedData.toList
  }

  lazy val random = new Random(42)
  override lazy val humanErrorDistribution = EpsilonRandomErrorDistribution(0.3, random)
  override lazy val humanDelayDistribution = ClippedGaussianHumanDelayDistribution(2000, 500, random)

  /**
   * This specifies the budget that this run will spend, in dollars. You may not use all of it, but the engine will stop
   * asking humans for help, and revert to simple machine learning, after it has exhausted the budget. This includes
   * money spent on retainers in order to recruit workers in the first place.
   *
   * @return amount in dollars to use as budget
   */
  override def budget: Double = 20.00
}

object CraigslistUseCase extends App {
  val craigslistUseCase = new CraigslistUseCase()

  def dumpData(data : List[(List[String],List[String])], name : String): Unit = {
    val folder = new File("results/"+craigslistUseCase.useCaseReportSubpath)
    if (!folder.exists()) folder.mkdirs()

    val file = new File("results/"+craigslistUseCase.useCaseReportSubpath+"/"+name+".txt")
    if (file.exists()) file.delete()
    if (!file.exists()) file.createNewFile()
    val bw = new BufferedWriter(new FileWriter(file))
    for (pair <- data) {
      bw.write("#"+data.indexOf(pair)+": ")
      for (token <- pair._1) {
        bw.write(token)
        bw.write(" ")
      }
      bw.write("\n")
    }
    bw.close()
  }

  dumpData(craigslistUseCase.trainSet, "train_data")
  dumpData(craigslistUseCase.devSet, "dev_data")
  dumpData(craigslistUseCase.testSet, "test_data")


  val poolSize = 10
  craigslistUseCase.testWithArtificialHumans(craigslistUseCase.devSet, craigslistUseCase.humanErrorDistribution, craigslistUseCase.humanDelayDistribution, 0.01, poolSize, "artificial_human")
  // craigslistUseCase.testBaselineForAllHuman(craigslistUseCase.devSet, 0.3, 2000, 500, 0.01, poolSize, 1) // 1 query baseline
  // craigslistUseCase.testBaselineForAllHuman(craigslistUseCase.devSet, 0.3, 2000, 500, 0.01, poolSize, 3) // 3 query baseline
  // craigslistUseCase.testBaselineForOfflineLabeling(craigslistUseCase.devSet)
  // craigslistUseCase.testWithRealHumans(craigslistUseCase.devSet, poolSize)
}
