package edu.stanford.lense_base.models

import edu.stanford.lense_base.humancompute.EpsilonRandomErrorDistribution

import scala.util.Random

/**
 * Created by keenon on 6/2/15.
 *
 * Finally getting serious about fixing the stupid Logistic model
 */
object LogisticTest extends App {
  lazy val rand = new Random(42)
  val humanErrorModel = new EpsilonRandomErrorDistribution(0.3, rand)

  val classes = List("true", "false")

  val logisticExternalStream : LogisticExternalModelStream[String] = new LogisticExternalModelStream[String](humanErrorModel, classes) {
    /**
     * Extract the features to use for logistic regression
     * @param input the input over which to extract features
     * @return
     */
    override def getFeatures(input: String): Map[String, Double] = Map(input -> 1.0)

    /**
     * Defines the possible output values of the model
     * @return
     */
    override def possibleValues: List[String] = classes
  }

  logisticExternalStream.train(List(("rabbit", "true"), ("bunny", "true"), ("hare", "true"), ("tortoise", "false"), ("turtle", "false"), ("shell", "false")))

  println("Prior for \"rabbit\" (should be true): "+logisticExternalStream.prior("rabbit"))
  println("Prior for \"bunny\" (should be true): "+logisticExternalStream.prior("bunny"))
  println("Prior for \"hare\" (should be true): "+logisticExternalStream.prior("hare"))
  println("Prior for \"tortoise\" (should be false): "+logisticExternalStream.prior("tortoise"))
  println("Prior for \"turtle\" (should be false): "+logisticExternalStream.prior("turtle"))
  println("Prior for \"shell\" (should be false): "+logisticExternalStream.prior("shell"))
}
