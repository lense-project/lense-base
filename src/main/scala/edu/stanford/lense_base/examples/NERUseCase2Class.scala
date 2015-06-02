package edu.stanford.lense_base.examples

import java.io.{FileWriter, BufferedWriter, File}

import edu.stanford.lense_base.gameplaying.{SamplingLookaheadOneHeuristic, GamePlayer}
import edu.stanford.lense_base.humancompute.{ConfusionMatrixErrorDistribution, ObservedHumanDelayDistribution}

/**
 * Created by keenon on 5/28/15.
 *
 * Settings for 2Class experiments
 */
class NERUseCase2Class extends NERUseCase {
  // Exclude ORG & MISC, cause they're confusing to Turkers
  override def legalTokens: Set[String] = Set("O", "PER", "LOC")

  lazy val yaml = loadTutorialYAML("src/main/resources/tutorials/ner-2-class.yaml")

  override def useCaseReportSubpath : String = "ner2class"

  override lazy val humanErrorDistribution = ConfusionMatrixErrorDistribution("data/ner/human_confusion_2class.csv", random)
  override lazy val humanDelayDistribution = ObservedHumanDelayDistribution("data/ner/human_latency_data.txt", random)

  override def budget: Double = 200.00

  override def lossFunction(sequence: List[String], mostLikelyGuesses: List[(Int, String, Double)], cost: Double, time: Double): Double = {
    val expectedErrors = mostLikelyGuesses.map{
      // We much prefer to not tag 0s incorrectly
      case (_,"0",p) => 1.0 - p
      // This is for non O predictions
      case triple => 1.0 - triple._3
    }.sum

    // A reduction in error of at least 10% for each cent spent
    expectedErrors + 10*cost
  }

  override def gamePlayer : GamePlayer = new SamplingLookaheadOneHeuristic(humanErrorDistribution,humanDelayDistribution)
}

object NERUseCase2Class extends App {
  val nerUseCase = new NERUseCase2Class()

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
  nerUseCase.testWithArtificialHumans(nerUseCase.data, nerUseCase.humanErrorDistribution, nerUseCase.humanDelayDistribution, 0.0025, poolSize, "artificial_human")
  // nerUseCase.testBaselineForAllHuman(nerUseCase.data, nerUseCase.humanErrorDistribution, nerUseCase.humanDelayDistribution, 0.0025, poolSize, 7, useRealHumans = true) // 1 query baseline
  // nerUseCase.testBaselineForAllHuman(nerUseCase.data, nerUseCase.humanErrorDistribution, nerUseCase.humanDelayDistribution, 0.0025, poolSize, 2) // 3 query baseline
  // nerUseCase.testBaselineForOfflineLabeling(nerUseCase.data)
  // nerUseCase.testWithRealHumans(nerUseCase.data, poolSize)
}
