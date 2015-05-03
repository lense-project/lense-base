package edu.stanford.lense_base

import edu.stanford.lense_base.graph.{GraphStream, Graph}

/**
 * Created by keenon on 5/2/15.
 *
 * This holds the abstract guts of a use case for Lense, which should help minimize crufty overhead when generating new
 * use cases
 */
abstract class LenseUseCase[Input <: AnyRef, Output <: AnyRef] {
  val graphStream : GraphStream = new GraphStream()

  /**
   * This function takes an Input, and an Output (which should be used to assign observedValue's if not null).
   * This must return a graph created in the local GraphStream.
   *
   * @param input the input that the graph will represent
   * @param output a possibly null correct output, that should be used to assign observedValue's to the graph
   * @return a graph representing the input, and taking labels from the output if it is passed in
   */
  def toGraph(input : Input, output : Output = Nil.asInstanceOf[Output]) : Graph

  /**
   * An opportunity to provide some seed data for training the model before the online task begins. This data will
   * be used to train the classifier prior to any testing or online activity
   *
   * @return model seed training data
   */
  def initialTrainingData : List[(Input, Output)] = List()

  /**
   * This will run a test against artificial humans, with a "probability epsilon choose uniformly at random" error
   * function. It will print results to stdout.
   *
   * @param goldPairs pairs of Input and the corresponding correct Output objects
   * @param humanErrorRate the error rate epsilon, so if 0.3, then with P=0.3 artificial humans will choose uniformly at random
   */
  def testWithArtificialHumans(goldPairs : List[(Input, Output)], humanErrorRate : Double) : Unit = {
    //TODO
  }

  /**
   * This will run a test against real live humans. The LenseUseCase needs to be triggered from inside Jetty, once it's
   * running, so that there's a server to ask for human opinions.
   *
   * @param goldPairs pairs of Input and the corresponding correct Output objects
   */
  def testWithRealHumans(goldPairs : List[(Input, Output)]) : Unit = {
    //TODO
  }
}
