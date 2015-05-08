package edu.stanford.lense_base

import edu.stanford.lense_base.graph.{FactorType, NodeType, GraphNode, Graph}
import edu.stanford.lense_base.humancompute.HumanComputeUnit

import scala.collection.mutable

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

  lazy val nodeType : NodeType = graphStream.makeNodeType(labelTypes)
  lazy val factorType : FactorType = graphStream.makeFactorType(List(nodeType, nodeType))

  /**
   * This function takes an Input
   * This must return a graph created in the local GraphStream, and it should create a GraphNodeQuestion for each node
   * in the created graph, in case we want to query humans about it.
   *
   * @param input the input that the graph will represent
   * @return a graph representing the input, and taking labels from the output if it is passed in
   */
  override def toGraph(input: List[String]): Graph = {
    val graph = graphStream.newGraph()
    val questionMap = mutable.Map[GraphNode, GraphNodeQuestion]()

    var lastNode : GraphNode = null
    for (i <- 0 to input.size-1) {
      val newNode = graph.makeNode(nodeType, featureExtractor(input, i), payload = (input,i), toString = "Node["+input(i)+"]")
      if (lastNode != null) {
        graph.makeFactor(factorType, List(lastNode, newNode))
      }
      lastNode = newNode
    }

    graph
  }

  override def getQuestion(node : GraphNode, hcu : HumanComputeUnit) : GraphNodeQuestion = {
    val pair = node.payload.asInstanceOf[(List[String],Int)]
    GraphNodeQuestion(getHumanQuestion(pair._1, pair._2), labelTypes.toList.map(l => new GraphNodeAnswer(getHumanVersionOfLabel(l), l)), hcu, node)
  }

  /**
   * Reads the MAP assignment out of the values object, and returns an Output corresponding to this graph having these
   * values.
   * The keys of the values map will always correspond one-to-one with the nodes of the graph.
   *
   * @param graph the graph, with observedValue's on all the nodes
   * @param values a map corresponding the nodes of the graph with their String labels
   * @return an Output version of this graph
   */
  override def toOutput(graph: Graph, values: Map[GraphNode, String]): List[String] = {
    graph.nodes.toList.sortBy(_.payload.asInstanceOf[(List[String],Int)]._2).map(n => values(n))
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
  override def lossFunction(mostLikelyGuesses: List[(GraphNode, String, Double)], cost: Double, time: Long): Double = {
    if (mostLikelyGuesses.size == 0) {
      lossFunction(List(), List(), cost, time)
    }
    else {
      val sentence = mostLikelyGuesses.head._1.payload.asInstanceOf[(List[String], Int)]._1
      val translatedGuesses = mostLikelyGuesses.map(triple => (triple._1.payload.asInstanceOf[(List[String], Int)]._2, triple._2, triple._3))
      lossFunction(sentence, translatedGuesses, cost, time)
    }
  }

  override def getCorrectLabel(node : GraphNode, goldOutput : List[String]) : String = {
    goldOutput(node.payload.asInstanceOf[(List[String],Int)]._2)
  }

  /**
   * A hook to be able to render intermediate progress during testWith[...] calls. Intended to print to stdout.
   */
  override def renderClassification(graph : Graph, goldMap : Map[GraphNode, String], guessMap : Map[GraphNode, String]) : Unit = {
    if (graph.nodes.size > 0) {
      val sequence = graph.nodes.head.payload.asInstanceOf[(List[String], Int)]._1
      println(graph.nodes.map(n => (n,n.payload.asInstanceOf[(List[String], Int)]._2)).sortBy(_._2)
        .map(n => sequence(n._2)+"(gold:"+goldMap(n._1)+",guess:"+guessMap(n._1)+")").mkString(" "))
    }
    else {
      println("Classified an empty graph")
    }
  }
}
