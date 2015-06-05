package edu.stanford.lense_base.examples

import java.io.{BufferedWriter, FileWriter, FileInputStream}
import java.util.Properties

import edu.stanford.lense_base.gameplaying.{SamplingLookaheadOneHeuristic, MCTSGamePlayer, ThresholdHeuristic, GamePlayer}
import edu.stanford.lense_base.humancompute.{ObservedErrorDistribution, EpsilonRandomErrorDistribution, ClippedGaussianHumanDelayDistribution}
import edu.stanford.lense_base.LenseMulticlassUseCase
import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.models._
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.neural.rnn.RNNCoreAnnotations
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.word2vec.Word2VecLoader
import org.yaml.snakeyaml.Yaml

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.Source
import scala.reflect.io.File
import scala.util.Random

import scala.collection.JavaConversions._

/**
 * Created by keenon on 5/20/15.
 *
 * Using the Stanford sentiment dataset for a simple turk-able question set
 */
class SentimentUseCase extends LenseMulticlassUseCase[String] {
  lazy val trainSet : List[(String,String)] = loadData("data/sentiment/aclImdb/train").take(20)
  lazy val fullTestSet : List[(String,String)] = loadData("data/sentiment/aclImdb/test") ++ loadData("data/sentiment/aclImdb/train").filter(p => !trainSet.contains(p))
  lazy val testSet : List[(String,String)] = fullTestSet.take(1800)
  lazy val devSet : List[(String,String)] = fullTestSet.filter(!testSet.contains(_)).take(400)

  lazy val word2vec : java.util.Map[String, Array[Double]] = try {
    Word2VecLoader.loadData("data/wordvectors/glove300.ser.gz")
    // new java.util.HashMap[String, Array[Double]]()
  } catch {
    case e : Throwable =>
      // Couldn't load word vectors
      System.err.println("*** COULDN'T LOAD WORD VECTORS")
      e.printStackTrace()
      // return an empty map
      new java.util.HashMap[String, Array[Double]]()
  }

  lazy val coreNLP : StanfordCoreNLP = {
    val props = new Properties()
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, parse, sentiment")
    new StanfordCoreNLP(props)
  }

  val useSocherEmbeddings = false

  override def labelTypes: List[String] = List("POS", "NEG")

  override def getHumanVersionOfLabel(label: String): String = label match {
    case "POS" => "Positive"
    case "NEG" => "Negative"
    case a => a
  }

  // The null class for during analysis
  override def defaultClass : String = "NEG"

  override def initialTrainingData : List[(String, String)] = trainSet

  val sentimentDevSet = devSet

  lazy val logisticModelStream : ModelStream = new LogisticExternalModelStream[String](humanErrorDistribution, labelTypes) {
    override def devPairs = sentimentDevSet

    val socherEmbeddingsCache = mutable.Map[String, Map[String, Double]]()

    def initializeEmbeddingsCache() = {
      for (line <- Source.fromFile("data/sentiment/socher_cache.txt").getLines()) {
        val parts = line.split("\t")
        if (parts.size > 1) {
          try {
            val input = parts(0)
            val features = parts.slice(1, parts.size).zipWithIndex.map(pair => "emb" + pair._2 -> java.lang.Double.parseDouble(pair._1)).toMap
            socherEmbeddingsCache.put(input, features)
          }
          catch {
            case e : Throwable => System.err.println("Failed to parse Socher cache line: "+line)
          }
        }
      }
      println("Recovered "+socherEmbeddingsCache.size+" embeddings")
    }

    initializeEmbeddingsCache()

    override def getFeatures(input: String): Map[String, Double] = {
      val embeddingFeatures = if (useSocherEmbeddings) {
        if (!socherEmbeddingsCache.contains(input)) {
          println("Parsing: "+input)
          val annotation: Annotation = new Annotation(input)
          coreNLP.annotate(annotation)
          val sentences = annotation.get(classOf[SentencesAnnotation])
          val embeddings = ListBuffer[Double]()
          embeddings.sizeHint(25)
          for (i <- 0 to 24) embeddings.append(0.0)

          for (sentence <- sentences) {
            val tree = sentence.get(classOf[SentimentCoreAnnotations.SentimentAnnotatedTree])
            val predictions = RNNCoreAnnotations.getNodeVector(tree)
            for (i <- 0 to 24) embeddings.set(i, embeddings.get(i) + predictions.get(i))
          }
          socherEmbeddingsCache.put(input, embeddings.zipWithIndex.map(pair => "emb:" + pair._2 -> pair._1).toMap)

          this.synchronized {
            val bw = new BufferedWriter(new FileWriter("data/sentiment/socher_cache.txt"))

            System.out.println("Flushing Socher embeddings to cache...")

            for (pair <- socherEmbeddingsCache) {
              bw.write(pair._1)
              bw.write("\t")
              val doubles = pair._2.toList.sortBy(_._2).map(_._2)
              for (d <- doubles) {
                bw.write(""+d)
                bw.write("\t")
              }
              bw.write("\n")
            }

            bw.close()
          }
        }
        socherEmbeddingsCache(input)
      }
      else {
        Map[String,Double]()
      }

      // Stupid bag of words features...
      val tokens = input.split(" ")
      val unigrams = tokens.map(tok => (tok, 1.0)).toMap
      val bigrams = (0 to tokens.size - 2).map(i => (tokens(i) + " "+ tokens(i+1), 1.0)).toMap
      val basicFeatures = unigrams ++ bigrams ++ embeddingFeatures

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
        (0 to normalized.length-1).map(i => "e" + i -> normalized(i)).toMap ++ basicFeatures
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
    "Please label this <b>movie review</b> as either <b>positive</b> or <b>negative</b>: <br><br><b>Movie Review:</b><div class='review'>"+input+"</div>"
  }

  /**context!!
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
    uncertainty + cost*1.0
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

  lazy val random = new Random()
  lazy val humanErrorDistribution = ObservedErrorDistribution("data/sentiment/context", "data/sentiment/confusion.csv", random)
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

  lazy val yaml = loadTutorialYAML("src/main/resources/tutorials/sentiment.yaml")

  override def getHumanTrainingExamples : List[(String, String, String)] = yaml._3
  override def humanTrainingIntroduction : String = yaml._1
  override def humanCheatSheet : String = yaml._2

  def loadTutorialYAML(path : String) : (String,String,List[(String, String, String)]) = {
    if (!File(path).exists) return ("","",List())
    val yaml = new Yaml()

    val doc : java.util.Map[String, Any] = yaml.load(new FileInputStream(path)).asInstanceOf[java.util.Map[String, Any]]

    val introText = doc.get("introduction").asInstanceOf[String]
    val cheatSheet = doc.get("cheat-sheet").asInstanceOf[String]

    val list = ListBuffer[(String, String, String)]()

    val exampleList : java.util.ArrayList[java.util.Map[String,String]] = doc.get("examples").asInstanceOf[java.util.ArrayList[java.util.Map[String,String]]]
    for (i <- 0 to exampleList.size()-1) {
      val example = exampleList.get(i)

      val review = example.get("review")
      val correctTag = example.get("correctTag")
      val comment = example.get("comment")

      list.+=((review, correctTag, comment))
    }

    (introText, cheatSheet, list.toList)
  }

  override def gamePlayer : GamePlayer = ThresholdHeuristic // new SamplingLookaheadOneHeuristic(humanErrorDistribution, humanDelayDistribution)

  def getContextForHumanErrorReplay(variable : ModelVariable, model : Model) : String = {
    variable.payload.asInstanceOf[String]
  }
}

object SentimentUseCase extends App {
  val sentimentUseCase = new SentimentUseCase()

  val poolSize = 4
  sentimentUseCase.testWithArtificialHumans(sentimentUseCase.testSet, sentimentUseCase.devSet, sentimentUseCase.humanErrorDistribution, sentimentUseCase.humanDelayDistribution, 0.01, poolSize, "artificial_human_threshold_log_reg")
  // sentimentUseCase.testBaselineForAllHuman(sentimentUseCase.testSet, sentimentUseCase.devSet, sentimentUseCase.humanErrorDistribution, sentimentUseCase.humanDelayDistribution, 0.01, poolSize, 1) // 1 query baseline
  // sentimentUseCase.testBaselineForAllHuman(sentimentUseCase.testSet, sentimentUseCase.devSet, sentimentUseCase.humanErrorDistribution, sentimentUseCase.humanDelayDistribution, 0.01, poolSize, 3) // 3 query baseline
  // sentimentUseCase.testBaselineForOfflineLabeling(sentimentUseCase.testSet, sentimentUseCase.devSet)
  // sentimentUseCase.testWithRealHumans(sentimentUseCase.testSet, sentimentUseCase.devSet, poolSize)
}