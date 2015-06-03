package edu.stanford.lense_base

import java.io.FileInputStream

import edu.stanford.lense_base.graph.{NodeType, Graph, GraphNode}
import edu.stanford.lense_base.humancompute.HumanComputeUnit
import edu.stanford.lense_base.models._
import edu.stanford.lense_base.server.{MulticlassTrainingQuestion, MulticlassQuestion, TrainingQuestion}
import org.yaml.snakeyaml.Yaml

import scala.collection.mutable.ListBuffer
import scala.reflect.io.File

/**
 * Created by keenon on 5/7/15.
 *
 * Makes it easy to build a multiclass classifier on top of L.E.N.S.E.
 *
 */
abstract class LenseMulticlassUseCase[Input] extends LenseUseCase[Input,String]{
  def labelTypes : List[String]
  def getHumanQuestion(input : Input) : String
  def getHumanVersionOfLabel(label : String) : String

  def univariateModelStream : UnivariateExternalModelStream[Input] = getModelStream.asInstanceOf[UnivariateExternalModelStream[Input]]

  /**
   * This gets the initial training examples to show to humans. Provide an input, the correct answer, and
   * any comments in HTML that you want displayed while the user is doing this example.
   *
   * @return
   */
  def getHumanTrainingExamples : List[(Input, String, String)]

  override def encodeModelWithValuesAsTSV(model : Model, values : Map[ModelVariable, String]) : String = {
    model.variables.head.payload.toString+"\t"+values(model.variables.head)+"\t"+model.getHumanObservationsForVariable(model.variables.head).map(triple => {
      triple._1+","+triple._2.getName+","+triple._3
    }).mkString("\t")
  }

  override def humanTrainingExamples : List[TrainingQuestion] = {
    getHumanTrainingExamples.map(triple => {
      MulticlassTrainingQuestion(getHumanQuestion(triple._1), labelTypes.toList.map(getHumanVersionOfLabel), getHumanVersionOfLabel(triple._2), triple._3)
    })
  }

  /**
   * This function takes an Input
   * This must return a graph created in the local GraphStream, and it should create a GraphNodeQuestion for each node
   * in the created graph, in case we want to query humans about it.
   *
   * @param input the input that the graph will represent
   * @return a graph representing the input, and taking labels from the output if it is passed in
   */
  override def toModel(input: Input): Model = {
    univariateModelStream.newModelForInput(input)
  }

  override def getQuestion(variable : ModelVariable, hcu: HumanComputeUnit): GraphNodeQuestion = {
    val input = variable.payload.asInstanceOf[Input]
    GraphNodeQuestion(getHumanQuestion(input), labelTypes.toList.map(l => new GraphNodeAnswer(getHumanVersionOfLabel(l), l)), hcu, variable)
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
  override def toOutput(model : Model, values: Map[ModelVariable, String]): String = {
    values(model.variables.head)
  }

  override def getCorrectLabel(variable : ModelVariable, goldOutput: String): String = goldOutput

}
