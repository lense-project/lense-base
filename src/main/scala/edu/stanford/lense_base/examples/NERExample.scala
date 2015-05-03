package edu.stanford.lense_base.examples

import edu.stanford.lense_base.LenseEngine
import edu.stanford.lense_base.gameplaying.{LookaheadOneHeuristic, GamePlayer, OneQuestionBaseline}
import edu.stanford.lense_base.graph.{Graph, GraphNode, GraphStream}
import edu.stanford.lense_base.gui.Java2DGUI
import edu.stanford.lense_base.server.{MulticlassQuestion, WorkUnitServlet}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.io.Source
import scala.util.{Try, Random}

import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by keenon on 4/27/15.
 *
 * Using hybrid crowd-ML to do accurate, cheap, fast NER
 */
abstract class NERExample(classes : Set[String]) {
  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => Promise[String]) : List[String]
}

class SingleQueryBaseline(classes : Set[String]) extends NERExample(classes) {
  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => Promise[String]) : List[String] = {
    (0 to tokenPOSPairs.size-1).map(simulateAskingHuman).map(p => Await.result(p.future, 0 nanos)).toList
  }
}

class MultiQueryBaseline(classes : Set[String], numQueries : Int) extends NERExample(classes) {
  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => Promise[String]) : List[String] = {
    (0 to tokenPOSPairs.size-1).map(i => {
      val map : mutable.HashMap[String,Int] = mutable.HashMap()
      for (cl <- classes) map.put(cl, 0)
      for (j <- 0 to numQueries) {
        val response = Await.result(simulateAskingHuman(i).future, 0 nanos)
        map.put(response, map(response)+1)
      }

      var largestClass = ""
      for (cl <- classes) if (map.getOrElse(largestClass, 0) < map(cl)) largestClass = cl
      largestClass
    }).toList
  }
}

class LenseFrameworkForNER(classes : Set[String],
                           gamePlayer : GamePlayer,
                           lossFunction : (List[(GraphNode, String, Double)], Double, Double) => Double,
                           trainingData : List[List[(String,String,String)]] = null) extends NERExample(classes) {
  val graphStream : GraphStream = new GraphStream()
  val nodeType = graphStream.makeNodeType(classes)
  val factorType = graphStream.makeFactorType(List(nodeType,nodeType))
  // This keeps state for learning, etc
  val lense : LenseEngine = new LenseEngine(graphStream, gamePlayer)

  if (trainingData != null) {
    println("Training with initial batch of data")
    lense.addTrainingData(trainingData.map(toGraph))

    var correctCount = 0.0
    var incorrectCount = 0.0
    val confusionMatrix : mutable.HashMap[(String,String),Int] = mutable.HashMap()

    // Test that we can overfit our training data
    for (t <- trainingData) {
      val graph = toGraph(t)
      /*
      println(graph.nodes.map(
        node => {
          val i = node.payload.asInstanceOf[Int]
          t(i)._1 + ":" + node.observedValue + ":" + t(i)._3
        }).mkString(" "))
        */

      for (node <- graph.nodes) {
        node.observedValue = null
      }
      val mapEstimate = graph.mapEstimate()
      mapEstimate.foreach(
        nodePair => {
          val i = nodePair._1.payload.asInstanceOf[Int]
          if (nodePair._2 == t(i)._3) {
            correctCount += 1
          }
          else {
            incorrectCount += 1
          }
          val pair = (nodePair._2, t(i)._3)

          confusionMatrix.put(pair, confusionMatrix.getOrElse(pair, 0) + 1)
        })

      /*
      println(mapEstimate.map(
        nodePair => {
          val i = nodePair._1.payload.asInstanceOf[Int]
          t(i)._1 + ":" + nodePair._2 + ":" + t(i)._3
        }).mkString(" "))
        */
    }

    println("Accuracy: "+(correctCount / (correctCount + incorrectCount)))
    println("Confusion: "+confusionMatrix)
  }

  def toGraph(tokenPOSPairs : List[Any]) : Graph = {
    val graph = graphStream.newGraph()

    var index = 0
    var lastNode : GraphNode = null
    for (tuple <- tokenPOSPairs) {
      val token = tuple match {
        case pair : (String,String) => pair._1
        case triple : (String,String,String) => triple._1
      }
      val pos = tuple match {
        case pair : (String,String) => pair._2
        case triple : (String,String,String) => triple._2
      }

      // This lets us optionally include an observed value, for initializing with training data
      val observed = tuple match {
        case pair : (String,String) => null
        case triple : (String,String,String) => triple._3
      }

      val capitalized = token.charAt(0).isUpper

      val features : Map[String, Double] = Map(
        "TOKEN:"+token -> 1.0,
        "POS:"+pos -> 1.0,
        "CAPITALIZED:"+capitalized -> 1.0,
        "BIAS" -> 0.0
      )

      val newNode = graph.makeNode(nodeType, features, payload = index, toString = index+":"+token, observedValue = observed)
      index += 1
      if (lastNode != null) {
        graph.makeFactor(factorType,List(lastNode, newNode))
      }
      lastNode = newNode
    }

    graph
  }

  // creates a GUI to use
  // println(new Java2DGUI(lense))

  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => Promise[String]) : List[String] = {
    val graph = toGraph(tokenPOSPairs)

    // We wrap this in a "Future" even though there's really no need to here, because in the general case
    // we want web-requests to have the option to function somewhat asynchronously
    def askHuman(node : GraphNode): Promise[String] = {
      simulateAskingHuman(node.payload.asInstanceOf[Int])
    }

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

  def testSystem(ner : NERExample,
                 data : List[List[(String,String,String)]],
                 epsilon : Double = 0.3,
                 askRealHuman : ((List[String], Int, List[String]) => Promise[String]) = null) : (Double,Int) = {
    val classes = data.flatMap(_.map(_._3)).distinct.toList
    val random = new Random(42)

    var correct : Double = 0
    var incorrect : Double = 0
    var numQueries : Int = 0
    val confusionMatrix : mutable.HashMap[(String,String), Int] = mutable.HashMap()

    for (sentence <- data) {
      def simulateQueryHuman(i : Int) : Promise[String] = {
        numQueries += 1
        val p = Promise[String]()
        p.complete(Try {
          // With probability epsilon we choose at random
          if (random.nextDouble() < epsilon) {
            classes(random.nextInt(classes.size))
          }
          // Otherwise we return the correct result
          else {
            sentence(i)._3
          }
        })
        p
      }

      def queryRealHuman(i : Int) : Promise[String] = {
        askRealHuman(sentence.map(_._1), i, classes)
      }

      println("Attempting to predict: "+sentence.map(_._1).mkString(" "))
      val predictedTags = ner.predictNER(sentence.map(tuple => (tuple._1, tuple._2)),
        // This is our "query human" function
        if (askRealHuman == null) simulateQueryHuman else queryRealHuman
      )
      println("Predicted: "+sentence.map(_._1).zip(predictedTags).map(pair => pair._1+":"+pair._2).mkString(" "))

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

  val allData = loadNER

  val data = allData.filter(_.size < 15).take(100)
  val trainSet = allData.filter(d => !data.contains(d)).take(100)

  println(data(1))
  val classes = (data ++ trainSet).flatMap(_.map(_._3)).distinct.toSet

  println(testSystem(new SingleQueryBaseline(classes), data, 0.3))
  println(testSystem(new MultiQueryBaseline(classes, 3), data, 0.3))

  def lossFunction(mostLikelyGuesses : List[(GraphNode,String,Double)], cost : Double, time : Double) : Double = {
    val expectedErrors = mostLikelyGuesses.map{
      // we much prefer to not tag 0s incorrectly
      case (_,"0",p) => (1.0 - p)*5.0
      case t => 1.0 - t._3
    }.sum
    expectedErrors*10 + cost
  }

  // println("One Question Baseline")
  // println(testSystem(new LenseFrameworkForNER(classes, OneQuestionBaseline, lossFunction, trainingData = trainSet), data, 0.3))

  println("Basic Lost Function LookaheadOneHeuristic")
  println(testSystem(new LenseFrameworkForNER(classes, LookaheadOneHeuristic, lossFunction, trainingData = trainSet), data, 0.3))

  def testWithRealHumans() = {
    println("****\n****\n****\nRunning a real NER test!")

    val allData = loadNER

    val data = loadNER.filter(_.size < 15).take(100)
    val trainSet = allData.filter(d => !data.contains(d)).take(100)

    println(data(1))
    val classes = (data++trainSet).flatMap(_.map(_._3)).distinct.toSet
    println("Classes: "+classes)

    def askRealHuman(sentence : List[String], i : Int, classes : List[String]) : Promise[String] = {

      var question = "Dear NLP Researcher:<br>What NER type is this?<br>"
      for (j <- 0 to sentence.length-1) {
        if (j > 0) question += " "
        if (i == j) question += "<b>[ "
        question += sentence(j)
        if (i == j) question += " ]</b>"
      }

      println("Asking human question: "+question)

      val p = Promise[String]()
      WorkUnitServlet.addWorkUnit(
        new MulticlassQuestion(
          question,
          classes,
          p
        )
      )
      p
    }

    println(testSystem(new LenseFrameworkForNER(classes, LookaheadOneHeuristic, lossFunction, trainingData = trainSet), data, 0.3, askRealHuman))
  }
}