package edu.stanford.lense_base.examples

import java.io.{FileWriter, BufferedWriter, File}

import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.humancompute.HumanComputeUnit
import edu.stanford.lense_base.{GraphNodeAnswer, GraphNodeQuestion, LenseSequenceUseCase}

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by keenon on 5/3/15.
 *
 * Does NER using sequence use cases
 */
class NERUseCase extends LenseSequenceUseCase {
  lazy val allData : List[(List[String],List[String])] = loadNER
  lazy val data : List[(List[String],List[String])] = allData.filter(_._1.size < 15).take(100) // .slice(48, 54)
  lazy val trainSet : List[(List[String],List[String])] = allData.filter(d => !data.contains(d)).take(0)
  // lazy val trainSet : List[(List[String],List[String])] = allData.filter(_._1.size < 15).take(48)

  override def initialTrainingData : List[(List[String], List[String])] = trainSet

  override def labelTypes: Set[String] = (data ++ trainSet).flatMap(_._2).distinct.toSet

  override def getHumanQuestion(sequence: List[String], i: Int): String = {
    var question = "Dear NLP Researcher:<br>What NER type is this?<br>"
    for (j <- 0 to sequence.length-1) {
      if (j > 0) question += " "
      if (i == j) question += "<b>[ "
      question += sequence(j)
      if (i == j) question += " ]</b>"
    }
    question
  }

  override def getHumanVersionOfLabel(label: String): String = "<b>"+label+"<\b>"

  override def lossFunction(sequence: List[String], mostLikelyGuesses: List[(Int, String, Double)], cost: Double, time: Double): Double = {
    val expectedErrors = mostLikelyGuesses.map{
      // we much prefer to not tag 0s incorrectly
      case (_,"0",p) => (1.0 - p)*5.0
      case t => 1.0 - t._3
    }.sum
    // This will trade 10 human labels for fully correct token
    expectedErrors*4 + cost
  }

  override def featureExtractor(sequence: List[String], i: Int): Map[String, Double] = {
    Map(
      "token:" + sequence(i).toLowerCase -> 1.0,
      "capitalized:" + (sequence(i)(0).isUpper && sequence(i).exists(_.isLower)) -> 1.0,
      "BIAS" -> 0.0
    )
  }

  override def useCaseReportSubpath : String = "ner"

  def loadNER : List[(List[String],List[String])] = {
    val loadedData : ListBuffer[(List[String],List[String])] = ListBuffer()
    val currentSentence : ListBuffer[String] = ListBuffer()
    val currentNER : ListBuffer[String] = ListBuffer()

    for (line <- Source.fromFile("data/conll.iob.4class.train").getLines()) {
      val parts = line.split("\t")
      if (parts.size == 4) {
        val word: String = parts(0)
        val pos: String = parts(1)
        val ner: String = parts(3)
        currentSentence.+=(word)
        currentNER.+=(ner)

        if (word == ".") {
          loadedData+=((currentSentence.toList, currentNER.toList))
          currentSentence.clear()
          currentNER.clear()
        }
      }
    }

    loadedData.toList
  }
}

object RunTestCase extends App {
  val nerUseCase = new NERUseCase()

  /*
  humanErrorRate : Double,
  humanDelayMean : Int,
  humanDelayStd : Int,
  workUnitCost : Double,
  startNumArtificialHumans : Int) : Unit = {
  */

  def dumpData(data : List[(List[String],List[String])], name : String): Unit = {
    val file = new File("results/"+name+".txt")
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

  dumpData(nerUseCase.data, "test_data")
  dumpData(nerUseCase.trainSet, "train_data")

  val poolSize = 10
  nerUseCase.testWithArtificialHumans(nerUseCase.data, 0.3, 2000, 500, 1.0, poolSize, "artificial_human")
  // nerUseCase.testBaselineForAllHuman(nerUseCase.data, 0.3, 2000, 500, 1.0, poolSize, 1) // 1 query baseline
  // nerUseCase.testBaselineForAllHuman(nerUseCase.data, 0.3, 2000, 500, 1.0, poolSize, 3) // 3 query baseline
  // nerUseCase.testBaselineForOfflineLabeling(nerUseCase.data)
  // nerUseCase.testWithRealHumans(nerUseCase.data)
}
