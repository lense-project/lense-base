package edu.stanford.lense_base

import edu.stanford.lense_base.graph.{Graph, GraphNode}
import edu.stanford.lense_base.humancompute.HumanComputeUnit
import edu.stanford.lense_base.server.{MulticlassTrainingQuestion, MulticlassQuestion, TrainingQuestion}

/**
 * Created by keenon on 5/7/15.
 *
 * Makes it easy to build a multiclass classifier on top of L.E.N.S.E.
 *
 */
abstract class LenseMulticlassUseCase[Input] extends LenseUseCase[Input,String]{
  def labelTypes : Set[String]
  def getFeatures(input : Input) : Map[String, Double]
  def getHumanQuestion(input : Input) : String
  def getHumanVersionOfLabel(label : String) : String

  /**
   * This gets the initial training examples to show to humans. Provide an input, the correct answer, and
   * any comments in HTML that you want displayed while the user is doing this example.
   *
   * @return
   */
  def getHumanTrainingExamples : List[(Input, String, String)] = List()

  lazy val nodeType = graphStream.makeNodeType(labelTypes)

  override def humanTrainingExamples : List[TrainingQuestion] = getHumanTrainingExamples.map(triple => {
    MulticlassTrainingQuestion(getHumanQuestion(triple._1), labelTypes.toList.map(getHumanVersionOfLabel), getHumanVersionOfLabel(triple._2), triple._3)
  })

  /**
   * This function takes an Input
   * This must return a graph created in the local GraphStream, and it should create a GraphNodeQuestion for each node
   * in the created graph, in case we want to query humans about it.
   *
   * @param input the input that the graph will represent
   * @return a graph representing the input, and taking labels from the output if it is passed in
   */
  override def toGraph(input: Input): Graph = {
    val graph = graphStream.newGraph()
    val node = graph.makeNode(nodeType, getFeatures(input), payload = input)
    if (graph.marginalEstimate().size != 1) throw new IllegalStateException("Seems marginals aren't computing correctly")
    graph
  }

  override def getQuestion(node: GraphNode, hcu: HumanComputeUnit): GraphNodeQuestion = {
    val input = node.payload.asInstanceOf[Input]
    GraphNodeQuestion(getHumanQuestion(input), labelTypes.toList.map(l => new GraphNodeAnswer(getHumanVersionOfLabel(l), l)), hcu, node)
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
  override def toOutput(graph: Graph, values: Map[GraphNode, String]): String = {
    values(graph.nodes.head)
  }

  override def getCorrectLabel(node: GraphNode, goldOutput: String): String = goldOutput
}
