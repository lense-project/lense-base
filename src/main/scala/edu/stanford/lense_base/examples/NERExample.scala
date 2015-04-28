package edu.stanford.lense_base.examples

import edu.stanford.lense_base.graph.GraphStream
import edu.stanford.lense_base.task.{Task, TaskStream}

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by keenon on 4/27/15.
 *
 * Using hybrid crowd-ML to do accurate, cheap, fast NER
 */
class NERExample(classes : Set[String]) {

  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => (String)) : List[String] = {
    // For now, super hack to test that human simulation is working
    (0 to tokenPOSPairs.size-1).map(simulateAskingHuman).toList
  }

}

object NERExample extends App {
  def loadNER : List[List[(String,String,String)]] = {
    val loadedData : ListBuffer[List[(String,String,String)]] = ListBuffer()
    val currentSentence : ListBuffer[(String,String,String)] = ListBuffer()

    for (line <- Source.fromFile("data/conll.iob.4class.train").getLines()) {
      val parts = line.split("\t")
      if (parts.size == 4) {
        val word: String = parts(0)
        val pos: String = parts(1)
        val ner: String = parts(3)
        currentSentence.+=((word, pos, ner))

        if (word == ".") {
          loadedData += currentSentence.toList
          currentSentence.clear()
        }
      }
    }

    loadedData.toList
  }

  def testSystem(data : List[List[(String,String,String)]], epsilon : Double) : Double = {
    val classes = data.flatMap(_.map(_._3)).distinct.toList
    val random = new util.Random(42)

    val ner = new NERExample(classes.toSet)

    var correct : Double = 0
    var incorrect : Double = 0

    for (sentence <- data) {
      val predictedTags = ner.predictNER(sentence.map(tuple => (tuple._1, tuple._2)), (i : Int) => {
        // With probability epsilon we choose at random
        if (random.nextDouble() < epsilon) {
          classes(random.nextInt(classes.size))
        }
        // Otherwise we return the correct result
        else {
          sentence(i)._3
        }
      })

      for (i <- 0 to sentence.size - 1) {
        if (sentence(i)._3 == predictedTags(i)) correct += 1
        else incorrect += 1
      }
    }

    correct / (correct + incorrect)
  }

  val data = loadNER
  println(data(1))

  println(testSystem(data, 0.1))
}