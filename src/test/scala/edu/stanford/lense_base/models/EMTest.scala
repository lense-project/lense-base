package edu.stanford.lense_base.models

import edu.stanford.lense_base.humancompute.{EpsilonRandomErrorDistribution, HumanErrorDistribution}

import scala.util.Random

/**
 * Created by keenon on 5/30/15.
 *
 * Tests EM against the GraphicalModelStream
 */
object EMTest extends App {
  val rand = new Random()
  val humanErrorDistribution : HumanErrorDistribution = new EpsilonRandomErrorDistribution(0.3, rand)
  val modelStream : GraphicalModelStream = new GraphicalModelStream(humanErrorDistribution)

  val nodeType = modelStream.graphStream.makeNodeType(Set("True", "False"))
  val factorType = modelStream.graphStream.makeFactorType(List(nodeType,nodeType))

  val g1 = modelStream.graphStream.newGraph()

  val n1 = g1.makeNode(nodeType)
  val n2 = g1.makeNode(nodeType)
  val f = g1.makeFactor(factorType, List(n1, n2))

  val model1 : GraphicalModel = modelStream.newModel()
  model1.setGraph(g1)

  val model2 = model1.cloneModelWithHumanObservation(model1.variables.head, "True")
  val model3 = model2.cloneModelWithHumanObservation(model1.variables.head, "False")

  modelStream.learn(List(model3, model3, model3))
}
