package edu.stanford.lense_base.task

import edu.stanford.lense_base.graph.GraphStream

/**
 * Created by keenon on 4/27/15.
 *
 * Users can specify a task stream here. This will perform learning across examples, etc
 */
class TaskStream {
  // A task stream comes with a GraphStream object to perform its learning, etc
  val graphStream : GraphStream = new GraphStream()

  def perform[ReturnType](task : Task[ReturnType]) : ReturnType = {
    task.taskCompleteCallback(Map())
  }
}
