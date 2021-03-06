package edu.stanford.lense_base.examples

import java.io.{FileWriter, BufferedWriter, File}
import java.util
import java.util.Properties

import cc.factorie.app.nlp.lemma.WordNetLemmatizer
import edu.stanford.lense_base.gameplaying.{ThresholdHeuristic, MCTSGamePlayer, LookaheadOneHeuristic, GamePlayer}
import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.humancompute.HumanComputeUnit
import edu.stanford.lense_base._
import edu.stanford.nlp.ie.NERFeatureFactory
import edu.stanford.nlp.ling.CoreAnnotations.{LemmaAnnotation, PartOfSpeechAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel}
import edu.stanford.nlp.pipeline.{MorphaAnnotator, Annotation, StanfordCoreNLP}
import edu.stanford.nlp.process.{WordShapeClassifier, CoreLabelTokenFactory}
import edu.stanford.nlp.sequences.{SeqClassifierFlags, Clique}
import edu.stanford.nlp.util.PaddedList
import edu.stanford.nlp.word2vec.Word2VecLoader

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
 * Created by keenon on 5/3/15.
 *
 * Does NER using sequence use cases
 */
abstract class NERUseCase extends LenseSequenceUseCase {

  // Exclude ORG & MISC, cause they're confusing to Turkers
  def legalTokens : Set[String]

  lazy val allData : List[(List[String],List[String])] = {
    random.shuffle(loadNER.filter(!_._2.exists(tok => !legalTokens.contains(tok))))
  }
  lazy val data : List[(List[String],List[String])] = allData.filter(_._1.size < 15).take(1000)
  lazy val trainSet : List[(List[String],List[String])] = allData.filter(d => !data.contains(d)).take(40) // .take(400)
  lazy val devSet : List[(List[String],List[String])] = allData.filter(d => !(data.contains(d) || trainSet.contains(d))).take(100) // .take(400)

  // lazy val trainSet : List[(List[String],List[String])] = allData.filter(_._1.size < 15).take(20)

  lazy val coreNLP : StanfordCoreNLP = {
    val props = new Properties()
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma")
    new StanfordCoreNLP(props)
  }

  lazy val word2vec : java.util.Map[String, Array[Double]] = if (useLearning) try {
    // Word2VecLoader.loadData("data/wordvectors/google-300.ser.gz")
    // Word2VecLoader.loadData("data/wordvectors/glove300.ser.gz")
    new java.util.HashMap[String, Array[Double]]()
  } catch {
    case e : Throwable =>
      // Couldn't load word vectors
      System.err.println("*** COULDN'T LOAD WORD VECTORS")
      e.printStackTrace()
      // return an empty map
      new java.util.HashMap[String, Array[Double]]()
  }
  else new java.util.HashMap[String,Array[Double]]()

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

  /**
   *
   * @param sequence tokens
   * @param mostLikelyGuesses (index in sequence, guess, confidence of guess)
   * @param cost money in dollars
   * @param time time in ms
   * @return
   */
  override def lossFunction(sequence: List[String], mostLikelyGuesses: List[(Int, String, Double)], cost: Double, time: Double): Double = {
    val expectedErrors = mostLikelyGuesses.map{
      // We much prefer to not tag 0s incorrectly
      case (_,"0",p) => 2*(1.0 - p)
      // This is for non O predictions
      case triple => 1.0 - triple._3
    }.sum

    // A reduction in error of at least 10% for each cent spent
    expectedErrors + 10*cost
  }

  override val maxLossPerNode : Double = {
    1.5
  }

  override def featureExtractor(sequence: List[String], i: Int): Map[String, Double] = {
    if (!useLearning) return Map()

    val annotation = new Annotation(sequence.mkString(" "))
    coreNLP.annotate(annotation)

    /*
    val flags = new SeqClassifierFlags()
    val featureFactory = new NERFeatureFactory[CoreLabel]()

    featureFactory.init(flags)

    val pad = new CoreLabelTokenFactory().makeToken()
    pad.setWord("OOB")
    pad.set(classOf[CoreAnnotations.AnswerAnnotation], "O")

    val tokens = annotation.get(classOf[TokensAnnotation])

    val paddedList = new PaddedList[CoreLabel](tokens, pad)
    val cliqueMe = Clique.valueOf(Array[Int](0))
    val cliqueMeLeft = Clique.valueOf(Array[Int](-1, 0))
    val features = featureFactory.getCliqueFeatures(paddedList, i, cliqueMe)

    println("Extracted features: "+features)
    */

    val word = sequence(i)
    val tokens = annotation.get(classOf[TokensAnnotation])
    /*
    println(sequence.mkString(" "))
    println(tokens)
    */

    var t = i
    for (j <- 0 to Math.min(tokens.size-1, i)) {
      val w : String = tokens.get(j).word()
      t -= Math.min(i-j, w.toCharArray.count(_ == 160.asInstanceOf[Char]))
    }
    // Prevent errors whenever possible by clipping
    if (t >= tokens.size) t = tokens.size-1
    /*
    if (i != t) {
      println("word [" + i + "]: " + sequence(i) + ", tokens[" + t + "]: " + tokens.get(t).word())
    }
    */

    def prefix(len : Int) = {
      word.substring(0, Math.min(Math.max(word.length-1,0), len))
    }

    def suffix(len : Int) = {
      word.substring(Math.max(0, word.length-len), Math.max(0,word.length-1))
    }

    val basicFeatures = Map(
      "token:" + sequence(i).toLowerCase -> 1.0,
      "lemma:" + tokens.get(t).get(classOf[LemmaAnnotation]) -> 1.0,
      "left-lemma:" + (if (t > 0) tokens.get(t-1).get(classOf[LemmaAnnotation]) else "#") -> 1.0,
      "left-left-lemma:" + (if (t > 1) tokens.get(t-2).get(classOf[LemmaAnnotation]) else "#") -> 1.0,
      "right-lemma:" + (if (t+1 < tokens.size) tokens.get(t+1).get(classOf[LemmaAnnotation]) else "$") -> 1.0,
      "right-right-lemma:" + (if (t+2 < tokens.size) tokens.get(t+2).get(classOf[LemmaAnnotation]) else "$") -> 1.0,
      "pos:" + tokens.get(t).get(classOf[PartOfSpeechAnnotation]) -> 1.0,
      "shape:" + WordShapeClassifier.wordShape(sequence(i), WordShapeClassifier.WORDSHAPECHRIS4) -> 1.0,
      "prefix1:" + prefix(1) -> 1.0,
      "prefix2:" + prefix(2) -> 1.0,
      "prefix3:" + prefix(3) -> 1.0,
      "suffix1:" + suffix(1) -> 1.0,
      "suffix2:" + suffix(2) -> 1.0,
      "suffix3:" + suffix(3) -> 1.0,
      "BIAS" -> 0.0
    )
    word2vec.get(sequence(i)) match {
      case vec : Array[Double] =>
        (0 to vec.length-1).map(i => "word2vec" + i -> vec(i)).toMap ++ basicFeatures
      case null => basicFeatures
    }
  }

  override def useCaseReportSubpath : String = "ner"

  def yaml : (String,String,List[(List[String], Int, String, String)])

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
  // override def gamePlayer : GamePlayer = LookaheadOneHeuristic
  // override def gamePlayer : GamePlayer = MCTSGamePlayer

  // Flag to skip learning when running with all human baselines
  // override lazy val useLearning : Boolean = false
}
