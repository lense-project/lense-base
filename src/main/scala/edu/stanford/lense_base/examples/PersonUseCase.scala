package edu.stanford.lense_base.examples

import java.io.{FileWriter, BufferedWriter, File}
import java.net.URL

import edu.stanford.lense_base.gameplaying.{ThresholdHeuristic, GamePlayer}
import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base._
import edu.stanford.lense_base.humancompute.{EpsilonRandomErrorDistribution, HumanErrorDistribution, ClippedGaussianHumanDelayDistribution, HumanDelayDistribution}

import scala.io.Source
import scala.util.Random

/**
 * Created by keenon on 5/26/15.
 *
 * Has Turkers distinguish between different people loaded from a database of images, uses basic ImageNet trained AlexNet
 * for embeddings of images, hopes that reasonably tuned transfer learning with be sufficient
 */
class PersonUseCase extends LenseMulticlassUseCase[PersonImage] {
  val celebrities = List("Daniel Craig", "Anderson Cooper", "Scarlett Johansson", "Miley Cyrus")

  lazy val dataSet = rand.shuffle(getCelebritiesSet(celebrities).map(p => (p, p.name)))

  lazy val trainSet : List[(PersonImage,String)] = List()
  lazy val testSet : List[(PersonImage,String)] = dataSet.filter(d => !trainSet.contains(d))

  override def labelTypes: Set[String] = celebrities.toSet

  override def getFeatures(input: PersonImage): Map[String, Double] = {
    val features = input.embedding.zipWithIndex.map(pair => {
      "nn:"+pair._2 -> pair._1
    }).toMap
    features
  }

  def loadDatabase() : List[PersonImage] = {
    val l = Source.fromFile("data/person_recognition/dev_urls.txt").getLines().toList
    l.slice(2, l.size).filter(_.split("\t").size > 2).map(line => {
      val parts = line.split("\t")
      PersonImage(parts(0), java.lang.Integer.parseInt(parts(1)), parts(2))
    })
  }

  def produceOrReadEmbeddingsForPerson(person : String, images : List[PersonImage]) = {
    val path = "data/person_recognition/"+person+" Embeddings.txt"
    val f = new File(path)
    val imagesOfThisPerson = images.filter(_.name == person)
    if (f.exists()) {
      val lines = Source.fromFile(path).getLines().toList
      imagesOfThisPerson.foreach(img =>
        if (img.index < lines.size && lines(img.index-1) != "err")
          img.embedding = lines(img.index-1).split(" ").map(d => java.lang.Double.parseDouble(d)).toList
      )
    }
    else {
      imagesOfThisPerson.foreach(img =>
        try {
          img.embedding = ImageEmbeddingsFromURL.embeddingsForURL(img.url)
        }
        catch {
          case e : Throwable => e.printStackTrace()
        }
      )
      val bw = new BufferedWriter(new FileWriter(path))
      for (i <- 1 to imagesOfThisPerson.size) {
        if (i > 1) bw.write("\n")
        bw.write({
          val images = imagesOfThisPerson.filter(_.index == i)
          if (images.size > 0) {
            val img = images.head
            if (img.embedding != null) img.embedding.mkString(" ")
            else "err"
          }
          else "err"
        })
      }
      bw.close()
    }
  }

  def getCelebritiesSet(names : List[String]) : List[PersonImage] = {
    val allImages = loadDatabase()
    for (name <- names) produceOrReadEmbeddingsForPerson(name, allImages)
    allImages.filter(p => names.contains(p.name) && p.embedding != null)
  }

  override def getHumanQuestion(input: PersonImage): String = "Who is this celebrity?<div style='width: 200px; height: 200px; background-color: black'><img src=\""+input.url+"\" style='width: 100%'></div>"

  override def getHumanVersionOfLabel(label: String): String = label

  lazy val rand = new Random()
  override def humanDelayDistribution: HumanDelayDistribution = ClippedGaussianHumanDelayDistribution(4000, 2000, rand)
  override def humanErrorDistribution: HumanErrorDistribution = EpsilonRandomErrorDistribution(0.1, rand)

  override def useCaseReportSubpath : String = "celebrity"

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

  /**
   * Some gameplayers care about losses being expressed as reward (negative loss) in the [0,1] range. To accomplish this,
   * we need to know a max loss per node
   * @return
   */
  override val maxLossPerNode : Double = {
    1.0
  }

  /**
   * This specifies the budget that this run will spend, in dollars. You may not use all of it, but the engine will stop
   * asking humans for help, and revert to simple machine learning, after it has exhausted the budget.
   *
   * @return amount in dollars to use as budget
   */
  override def budget: Double = 20.0

  override def gamePlayer : GamePlayer = ThresholdHeuristic
}

case class PersonImage(name : String, index : Int, url : String) {
  var embedding : List[Double] = null
}

/**
 * Created by keenon on 5/26/15.
 *
 * Runs against Caffe. Basically a bunch of system calls, some parsing, and voilla
 */
object ImageEmbeddingsFromURL {
  import sys.process._

  def embeddingsForURL(url : String) : List[Double] = {
    val name = url.split("/").last
    val download = "curl "+url+" > /tmp/"+name
    println("Running: "+download)
    (new URL(url) #> new File("/tmp/"+name)).!!
    val getEmbeddings = "src/main/python/run_embedding.sh /tmp/"+name
    println("Running: "+getEmbeddings)
    val output = getEmbeddings.!!.split("\\s").toList
    output.slice(1, output.size).filter{
      case d : String => d.length() > 0
    }.map(d => java.lang.Double.parseDouble(d.replaceAll("]","").trim()))
  }

  def main (args: Array[String]) {
    println(embeddingsForURL("http://1.bp.blogspot.com/_Y7rzCyUABeI/SNIltEyEnjI/AAAAAAAABOg/E1keU_52aFc/s400/ash_abhishek_365x470.jpg"))
  }
}

object PersonUseCase {
  def main(args: Array[String]) {
    val personUseCase = new PersonUseCase()

    val poolSize = 10
    personUseCase.testWithArtificialHumans(personUseCase.testSet, personUseCase.humanErrorDistribution, personUseCase.humanDelayDistribution, 0.01, poolSize, "artificial_human")
    // personUseCase.testBaselineForAllHuman(personUseCase.testSet, 0.3, 2000, 500, 0.01, poolSize, 1) // 1 query baseline
    // personUseCase.testBaselineForAllHuman(personUseCase.testSet, 0.3, 2000, 500, 0.01, poolSize, 3) // 3 query baseline
    // personUseCase.testBaselineForOfflineLabeling(personUseCase.testSet)
    // personUseCase.testWithRealHumans(personUseCase.testSet, poolSize)
  }
}
