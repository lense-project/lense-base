package edu.stanford.lense_base.examples

import edu.stanford.lense_base.Lense
import edu.stanford.lense_base.gameplaying.{LookaheadOneHeuristic, GamePlayer, OneQuestionBaseline}
import edu.stanford.lense_base.graph.{GraphNode, GraphStream}
import edu.stanford.lense_base.gui.Java2DGUI

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by keenon on 4/27/15.
 *
 * Using hybrid crowd-ML to do accurate, cheap, fast NER
 */
abstract class NERExample(classes : Set[String]) {
  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => (String)) : List[String]
}

class SingleQueryBaseline(classes : Set[String]) extends NERExample(classes) {
  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => (String)) : List[String] = {
    (0 to tokenPOSPairs.size-1).map(simulateAskingHuman).toList
  }
}

class MultiQueryBaseline(classes : Set[String], numQueries : Int) extends NERExample(classes) {
  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => (String)) : List[String] = {
    (0 to tokenPOSPairs.size-1).map(i => {
      val map : mutable.HashMap[String,Int] = mutable.HashMap()
      for (cl <- classes) map.put(cl, 0)
      for (j <- 0 to numQueries) {
        val response = simulateAskingHuman(i)
        map.put(response, map(response)+1)
      }

      var largestClass = ""
      for (cl <- classes) if (map.getOrElse(largestClass, 0) < map(cl)) largestClass = cl
      largestClass
    }).toList
  }
}

class LenseFramework(classes : Set[String], gamePlayer : GamePlayer, lossFunction : (List[(GraphNode, String, Double)], Double, Double) => Double) extends NERExample(classes) {
  val graphStream : GraphStream = new GraphStream()
  val nodeType = graphStream.makeNodeType(classes)
  val factorType = graphStream.makeFactorType(List(nodeType,nodeType))
  // This keeps state for learning, etc
  val lense : Lense = new Lense(graphStream, gamePlayer)

  // creates a GUI to use
  println(new Java2DGUI(lense))

  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => (String)) : List[String] = {
    val graph = graphStream.newGraph()

    var index = 0
    for (pair <- tokenPOSPairs) {
      val token = pair._1
      val pos = pair._2
      val capitalized = pair._1.charAt(0).isUpper

      val features : Map[String, Double] = Map(
        "TOKEN:"+token -> 1.0,
        "POS:"+pos -> 1.0,
        "CAPITALIZED:"+capitalized -> 1.0
      )

      graph.makeNode(nodeType, features, payload = index, toString = index+":"+token)
      index += 1
    }
    def askHuman(node : GraphNode): String = simulateAskingHuman(node.payload.asInstanceOf[Int])

    val assignments : Map[GraphNode, String] = lense.predict(graph,
      askHuman,
      lossFunction)

    assignments.toList.sortBy(_._1.payload.asInstanceOf[Int]).map(_._2)
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

  def testSystem(ner : NERExample, data : List[List[(String,String,String)]], epsilon : Double) : (Double,Int) = {
    val classes = data.flatMap(_.map(_._3)).distinct.toList
    val random = new util.Random(42)

    var correct : Double = 0
    var incorrect : Double = 0
    var numQueries : Int = 0
    val confusionMatrix : mutable.HashMap[(String,String), Int] = mutable.HashMap()

    for (sentence <- data) {
      val predictedTags = ner.predictNER(sentence.map(tuple => (tuple._1, tuple._2)),
      // This is our "query human" function
      (i : Int) => {
        numQueries += 1
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

        val key = (sentence(i)._3, predictedTags(i))
        confusionMatrix.put(key, confusionMatrix.getOrElse(key, 0) + 1)
      }
    }

    println(confusionMatrix)
    (correct / (correct + incorrect), numQueries)
  }

  val data = loadNER.filter(_.size < 15).take(100)
  println(data(1))
  val classes = data.flatMap(_.map(_._3)).distinct.toSet

  println(testSystem(new SingleQueryBaseline(classes), data, 0.3))
  println(testSystem(new MultiQueryBaseline(classes, 3), data, 0.3))

  def lossFunction(mostLikelyGuesses : List[(GraphNode,String,Double)], cost : Double, time : Double) : Double = {
    val expectedErrors = mostLikelyGuesses.map{
      // we much prefer to not tag 0s incorrectly
      case (_,"0",p) => (1.0 - p)*5.0
      case t => 1.0 - t._3
    }.sum
    expectedErrors + cost
  }

  println("One Question Baseline, with only BIAS feature")
  println(testSystem(new LenseFramework(classes, OneQuestionBaseline, lossFunction), data, 0.3))
  println("Basic Lost Function LookaheadOneHeuristic")
  println(testSystem(new LenseFramework(classes, LookaheadOneHeuristic, lossFunction), data, 0.3))
}