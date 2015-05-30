package edu.stanford.lense_base

import java.io.{FileInputStream, FileReader}
import java.nio.charset.MalformedInputException

import edu.stanford.lense_base.graph.{FactorType, NodeType, GraphNode, Graph}
import edu.stanford.lense_base.humancompute.HumanComputeUnit
import edu.stanford.lense_base.models._
import edu.stanford.lense_base.server.{TrainingQuestion, MulticlassTrainingQuestion}
import org.yaml.snakeyaml.Yaml

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.io.File

/**
 * Created by keenon on 5/3/15.
 *
 * Sequence labeling is a common use case for Lense, so it's nice to encapsulate that behavior in one place.
 *
 * Can be used for semantic slot filling, NER, and other useful NLP stuff
 */
abstract class LenseSequenceUseCase extends LenseUseCase[List[String],List[String]] {

  // The abstract stuff you have to override

  def labelTypes : Set[String]
  def featureExtractor(sequence : List[String], i : Int) : Map[String, Double]
  def getHumanQuestion(sequence : List[String], i : Int) : String
  def getHumanVersionOfLabel(label : String) : String
  def lossFunction(sequence : List[String], mostLikelyGuesses: List[(Int, String, Double)], cost: Double, time: Double) : Double

  /**
   * This gets the initial training examples to show to humans. Provide a sequence, an index, the correct answer, and
   * any comments in HTML that you want displayed while the user is doing this example.
   *
   * @return
   */
  def getHumanTrainingExamples : List[(List[String], Int, String, String)] = List()

  /**
   * If there's a YAML formatted tutorial someplace, then this is the key.
   *
   * @param path
   * @return
   */
  def loadTutorialYAML(path : String) : (String,String,List[(List[String], Int, String, String)]) = {
    if (!File(path).exists) return ("","",List())
    val yaml = new Yaml()

    val doc : java.util.Map[String, Any] = yaml.load(new FileInputStream(path)).asInstanceOf[java.util.Map[String, Any]]

    val introText = doc.get("introduction").asInstanceOf[String]
    val cheatSheet = doc.get("cheat-sheet").asInstanceOf[String]

    val list = ListBuffer[(List[String], Int, String, String)]()

    val exampleList : java.util.ArrayList[java.util.Map[String,String]] = doc.get("examples").asInstanceOf[java.util.ArrayList[java.util.Map[String,String]]]
    for (i <- 0 to exampleList.size()-1) {
      val example = exampleList.get(i)

      val sequence = example.get("sequence")
      val seqParts = sequence.split(" ")
      val selectedTokens = seqParts.zipWithIndex.filter(tokIndex => tokIndex._1.startsWith("[") && tokIndex._1.endsWith("]"))
      if (selectedTokens.size != 1) {
        throw new IllegalStateException("Input sequence must have *exactly one* token selected with []: Instead we got "+selectedTokens.size+": "+selectedTokens.map(_._1).toString)
      }
      val index = selectedTokens.head._2
      val finalTokens = seqParts.zipWithIndex.map(pair => {
        if (pair._2 == index) pair._1.substring(1, pair._1.size-1)
        else pair._1
      }).toList
      val correctTag = example.get("correctTag")
      val comment = example.get("comment")

      list.+=((finalTokens, index, correctTag, comment))
    }

    (introText, cheatSheet, list.toList)
  }

  lazy val graphicalModelStream : GraphicalModelStream = new GraphicalModelStream(humanErrorDistribution)

  override def getModelStream : ModelStream = graphicalModelStream

  lazy val nodeType : NodeType = graphicalModelStream.graphStream.makeNodeType(labelTypes)
  lazy val factorType : FactorType = graphicalModelStream.graphStream.makeFactorType(List(nodeType, nodeType))

  override def encodeModelWithValuesAsTSV(model : Model, values : Map[ModelVariable, String]) : String = {
    val m = model.asInstanceOf[GraphicalModel]
    m.variables.sortBy(_.payload.asInstanceOf[(List[String],Int)]._2).map(n => {
      val payload = n.payload.asInstanceOf[(List[String],Int)]
      payload._1(payload._2) + "\t" + values(n)
    }).mkString("\n")
  }

  override def humanTrainingExamples : List[TrainingQuestion] = getHumanTrainingExamples.map(quad => {
    MulticlassTrainingQuestion(getHumanQuestion(quad._1, quad._2), labelTypes.toList.map(l => getHumanVersionOfLabel(l)), getHumanVersionOfLabel(quad._3), quad._4)
  })

  /**
   * This function takes an Input
   * This must return a graph created in the local GraphStream, and it should create a GraphNodeQuestion for each node
   * in the created graph, in case we want to query humans about it.
   *
   * @param input the input that the graph will represent
   * @return a graph representing the input, and taking labels from the output if it is passed in
   */
  override def toModel(input: List[String]): Model = {
    val graph = graphicalModelStream.graphStream.newGraph(input.mkString(" "))

    var lastNode : GraphNode = null
    for (i <- 0 to input.size-1) {
      val newNode = graph.makeNode(nodeType, featureExtractor(input, i), payload = (input,i), toString = "Node["+input(i)+"]")
      if (lastNode != null) {
        graph.makeFactor(factorType, List(lastNode, newNode))
      }
      lastNode = newNode
    }

    val model = graphicalModelStream.newModel()
    model.setGraph(graph)
    model
  }

  override def getQuestion(variable : ModelVariable, hcu : HumanComputeUnit) : GraphNodeQuestion = {
    val pair = variable.asInstanceOf[GraphicalModelVariable].payload.asInstanceOf[(List[String],Int)]
    GraphNodeQuestion(getHumanQuestion(pair._1, pair._2), labelTypes.toList.map(l => new GraphNodeAnswer(getHumanVersionOfLabel(l), l)), hcu, variable)
  }

  /**
   * Reads the MAP assignment out of the values object, and returns an Output corresponding to this graph having these
   * values.
   * The keys of the values map will always correspond one-to-one with the nodes of the graph.
   *
   * @param model the graph, with observedValue's on all the nodes
   * @param values a map corresponding the nodes of the graph with their String labels
   * @return an Output version of this graph
   */
  override def toOutput(model : Model, values: Map[ModelVariable, String]): List[String] = {
    model.asInstanceOf[GraphicalModel].variables.toList.sortBy(_.payload.asInstanceOf[(List[String],Int)]._2).map(n => values(n))
  }

  /**
   * A way to define the loss function for you system. mostLikelyGuesses is a list of all the nodes being chosen on,
   * with their corresponding most likely label, and the probability the model assigns to the label.
   *
   * TODO: more docs here
   *
   * @param mostLikelyGuesses
   * @param cost
   * @param time
   * @return
   */
  override def lossFunction(mostLikelyGuesses: List[(ModelVariable, String, Double)], cost: Double, time: Long): Double = {
    val sentence = mostLikelyGuesses.head._1.asInstanceOf[GraphicalModelVariable].payload.asInstanceOf[(List[String], Int)]._1
    val translatedGuesses = mostLikelyGuesses.map(triple => (triple._1.asInstanceOf[GraphicalModelVariable].payload.asInstanceOf[(List[String], Int)]._2, triple._2, triple._3))
    lossFunction(sentence, translatedGuesses, cost, time)
  }

  override def getCorrectLabel(variable : ModelVariable, goldOutput : List[String]) : String = {
    goldOutput(variable.asInstanceOf[GraphicalModelVariable].payload.asInstanceOf[(List[String],Int)]._2)
  }

  /**
   * A hook to be able to render intermediate progress during testWith[...] calls. Intended to print to stdout.
   */
  override def renderClassification(model : Model, goldMap : Map[ModelVariable, String], guessMap : Map[ModelVariable, String]) : Unit = {
    val m = model.asInstanceOf[GraphicalModel]
    if (m.variables.size > 0) {
      val sequence = m.variables.head.payload.asInstanceOf[(List[String], Int)]._1
      println(m.variables.map(n => (n,n.payload.asInstanceOf[(List[String], Int)]._2)).sortBy(_._2)
        .map(n => sequence(n._2)+"(gold:"+goldMap(n._1)+",guess:"+guessMap(n._1)+")").mkString(" "))
    }
    else {
      println("Classified an empty graph")
    }
  }
}
