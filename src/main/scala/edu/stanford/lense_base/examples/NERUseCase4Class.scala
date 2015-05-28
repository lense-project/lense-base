package edu.stanford.lense_base.examples

import java.io.{FileWriter, BufferedWriter, File}

import edu.stanford.lense_base.{ObservedHumanDelayDistribution, ConfusionMatrixErrorDistribution}

/**
 * Created by keenon on 5/28/15.
 */
class NERUseCase4Class extends NERUseCase {
  // Exclude ORG & MISC, cause they're confusing to Turkers
  override def legalTokens: Set[String] = Set("O", "PER", "LOC", "ORG", "MISC")

  lazy val yaml = loadTutorialYAML("src/main/resources/tutorials/ner.yaml")

  override def useCaseReportSubpath : String = "ner4class"

  override lazy val humanErrorDistribution = ConfusionMatrixErrorDistribution("data/ner/human_confusion_data.csv", random)
  override lazy val humanDelayDistribution = ObservedHumanDelayDistribution("data/ner/human_latency_data.txt", random)
}

object NERUseCase4Class extends App {
  val nerUseCase = new NERUseCase4Class()

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
  // nerUseCase.testWithArtificialHumans(nerUseCase.data, nerUseCase.humanErrorDistribution, nerUseCase.humanDelayDistribution, 0.01, poolSize, "artificial_human")
  // nerUseCase.testBaselineForAllHuman(nerUseCase.data, nerUseCase.humanErrorDistribution, nerUseCase.humanDelayDistribution, 0.01, poolSize, 1) // 1 query baseline
  // nerUseCase.testBaselineForAllHuman(nerUseCase.data, nerUseCase.humanErrorDistribution, nerUseCase.humanDelayDistribution, 0.01, poolSize, 3) // 3 query baseline
  nerUseCase.testBaselineForOfflineLabeling(nerUseCase.data)
  // nerUseCase.testWithRealHumans(nerUseCase.data, poolSize)
}

