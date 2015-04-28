package edu.stanford.lense_base.task

import edu.stanford.lense_base.graph.GraphStream

/**
 * Created by keenon on 4/27/15.
 *
 * This is how users specify a task, designed to be flexible for varying loss functions across the same GraphStream, etc
 */
abstract class Task[ReturnType](taskStream : TaskStream) {
  def populateGraph(graph : taskStream.graphStream.Graph)
  def taskCompleteCallback(assignments : Map[taskStream.graphStream.Graph#Node, String]) : ReturnType

  // These are defined at the task level to default to the TaskStream values, but can be overridden if finer grained
  // control is required over the lossFunction and human querying process.
  def lossFunction(msDelay : Double, dollars : Double, errorProbability : Map[taskStream.graphStream.Graph#Node,Double]) : Double
  def queryHuman(node : taskStream.graphStream.Graph#Node) : String
}
