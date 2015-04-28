package edu.stanford.lense_base.examples

import edu.stanford.lense_base.graph.GraphStream
import edu.stanford.lense_base.task.{Task, TaskStream}

import scala.collection.mutable.ListBuffer

/**
 * Created by keenon on 4/27/15.
 *
 * Using hybrid crowd-ML to do accurate, cheap, fast NER
 */
class NERExample(classes : Set[String]) {

  val taskStream = new TaskStream()

  val nerLabelNodeType = taskStream.graphStream.makeNodeType(classes)
  val nerNeighborFactorType = taskStream.graphStream.makeFactorType(List(nerLabelNodeType, nerLabelNodeType))

  def predictNER(tokenPOSPairs : List[(String, String)], simulateAskingHuman : (Int) => (String)) : List[String] = {

    // Create a Task for the NER example

    val task = new Task[List[String]](taskStream) {
      val nodes : ListBuffer[GraphStream#Graph#Node] = ListBuffer()

      override def populateGraph(graph: GraphStream#Graph): Unit = {
        var lastNode : graph.Node = null
        for (i <- 0 to tokenPOSPairs.size-1) {
          val newNode = graph.Node(nodeType = nerLabelNodeType, features = Map("feat1" -> 1.0), payload = i)
          if (lastNode != null) {
            graph.Factor(nodes = List(lastNode, newNode), factorType = nerNeighborFactorType)
          }
          nodes += newNode
          lastNode = newNode
        }
      }

      override def taskCompleteCallback(assignments: Map[GraphStream#Graph#Node, String]): List[String] = {
        // Do something with the answers
        nodes.map(assignments(_)).toList
      }

      override def lossFunction(msDelay: Double,
                                dollars: Double,
                                errorProbability: Map[GraphStream#Graph#Node, Double]): Double = {
        dollars + errorProbability.values.sum
      }

      override def queryHuman(node: GraphStream#Graph#Node): String = {
        val index = node.payload.asInstanceOf[Int]
        simulateAskingHuman(index)
      }
    }

    taskStream.perform(task)
  }

}

object NERExample {

}