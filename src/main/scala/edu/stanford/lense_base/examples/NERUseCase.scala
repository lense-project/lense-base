package edu.stanford.lense_base.examples

import java.io.{FileWriter, BufferedWriter, File}

import edu.stanford.lense_base.gameplaying.{ThresholdHeuristic, MCTSGamePlayer, LookaheadOneHeuristic, GamePlayer}
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
 * Does NER using sequence use cases
 */
class NERUseCase extends LenseSequenceUseCase {

  // Exclude ORG & MISC, cause they're confusing to Turkers
  lazy val legalTokens = Set("O","PER","LOC")

  lazy val allData : List[(List[String],List[String])] = {
    random.shuffle(loadNER.filter(!_._2.exists(tok => !legalTokens.contains(tok))))
  }
  lazy val data : List[(List[String],List[String])] = allData.filter(_._1.size < 15).take(1000) // .slice(20, 50)
  lazy val trainSet : List[(List[String],List[String])] = allData.filter(d => !data.contains(d)).take(200)
  // lazy val trainSet : List[(List[String],List[String])] = allData.filter(_._1.size < 15).take(20)

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

  override def initialTrainingData : List[(List[String], List[String])] = trainSet

  override def labelTypes: Set[String] = (data ++ trainSet).flatMap(_._2).distinct.toSet

  override def getHumanQuestion(sequence: List[String], i: Int): String = {
    var question = "What type of thing is the bolded word?<br>"
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
    case "ORG" => "Organization"
    case "MISC" => "Miscellaneous"
    case "LOC" => "Location"
    case "PER" => "Person"
    case "O" => "None of the above"
    case a => a
  }

  override def lossFunction(sequence: List[String], mostLikelyGuesses: List[(Int, String, Double)], cost: Double, time: Double): Double = {
    val expectedErrors = mostLikelyGuesses.map{
      // we much prefer to not tag 0s incorrectly
      case (_,"0",p) => 1.0 - p
      case t => 1.0 - t._3
    }.sum

    // A reduction in error of at least 1% for each cent spent
    expectedErrors + cost*1
  }

  override val maxLossPerNode : Double = {
    1.5
  }

  override def featureExtractor(sequence: List[String], i: Int): Map[String, Double] = {
    val basicFeatures = Map(
      "token:" + sequence(i).toLowerCase -> 1.0,
      "capitalized:" + (if (sequence(i).length() > 0) sequence(i)(0).isUpper && sequence(i).exists(_.isLower) else false) -> 1.0,
      "left-word:" + (if (i > 0) sequence(i-1) else "#") -> 1.0,
      "right-word:" + (if (i < sequence.size-1) sequence(i+1) else "$") -> 1.0,
      "BIAS" -> 0.0
    )
    word2vec.get(sequence(i)) match {
      case vec : Array[Double] =>
        (0 to vec.length-1).map(i => "word2vec" + i -> vec(i)).toMap ++ basicFeatures
      case null => basicFeatures
    }
  }

  override def useCaseReportSubpath : String = "ner"

  lazy val yaml = loadTutorialYAML("src/main/resources/tutorials/ner-2-class.yaml")

  override def getHumanTrainingExamples : List[(List[String], Int, String, String)] = yaml._3
  override def humanTrainingIntroduction : String = yaml._1
  override def humanCheatSheet : String = yaml._2

  def loadNER : List[(List[String],List[String])] = {
    val loadedData : ListBuffer[(List[String],List[String])] = ListBuffer()
    val currentSentence : ListBuffer[String] = ListBuffer()
    val currentNER : ListBuffer[String] = ListBuffer()

    for (line <- Source.fromFile("data/ner/conll.iob.4class.train").getLines()) {
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

  // The null class for during analysis
  override def defaultClass : String = "O"

  lazy val random = new Random(42)
  // override lazy val humanErrorDistribution = ConfusionMatrixErrorDistribution("data/ner/human_confusion_data_no_org.csv", random)
  override lazy val humanErrorDistribution = EpsilonRandomErrorDistribution(0.3, random)
  override lazy val humanDelayDistribution = ObservedHumanDelayDistribution("data/ner/human_latency_data.txt", random)

  /**
   * This specifies the budget that this run will spend, in dollars. You may not use all of it, but the engine will stop
   * asking humans for help, and revert to simple machine learning, after it has exhausted the budget. This includes
   * money spent on retainers in order to recruit workers in the first place.
   *
   * @return amount in dollars to use as budget
   */
  override def budget: Double = 20.00


  /**
   * Override this to test with different game players
   *
   * @return a game player
   */
  override def gamePlayer : GamePlayer = ThresholdHeuristic
  // override def gamePlayer : GamePlayer = MCTSGamePlayer
}

object NERUseCase extends App {
  val nerUseCase = new NERUseCase()

  def dumpData(data : List[(List[String],List[String])], name : String): Unit = {
    val folder = new File("results/"+nerUseCase.useCaseReportSubpath)
    if (!folder.exists()) folder.mkdirs()

    val file = new File("results/"+nerUseCase.useCaseReportSubpath+"/"+name+".txt")
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

  val poolSize = 3
  nerUseCase.testWithArtificialHumans(nerUseCase.data, nerUseCase.humanErrorDistribution, nerUseCase.humanDelayDistribution, 0.01, poolSize, "artificial_human")
  // nerUseCase.testBaselineForAllHuman(nerUseCase.data, 0.3, 2000, 500, 0.01, poolSize, 1) // 1 query baseline
  // nerUseCase.testBaselineForAllHuman(nerUseCase.data, 0.3, 2000, 500, 0.01, poolSize, 3) // 3 query baseline
  // nerUseCase.testBaselineForOfflineLabeling(nerUseCase.data)
  // nerUseCase.testWithRealHumans(nerUseCase.data, poolSize)
}
