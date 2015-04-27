package edu.stanford.lense_base.graph

import scala.util.Random

/**
 * Test section, to proof the API for sensical tasks
 *
 * To start a new type of inference problem, create a new Interface().
 *
 * You can populate the Interface with NodeType() and ArcType() objects, which are the mechanism for weight-sharing
 * on the networks you will construct.
 *
 * You can have unary priors learned for nodes, by giving the nodes features.
 * You can also learn more complex feature-based priors for the factors, by giving them features.
 */

object BasicMap extends App {
  val simpleMapTest = new GraphStream()
  val nodeType = simpleMapTest.makeNodeType(Set("true", "false"),
    Map(
      "true" -> Map("feat1" -> 1.0, "feat2" -> -1.0),
      "false" -> Map("feat1" -> -1.0, "feat2" -> 1.0)
    )
  )

  // Create the graph

  val g = new simpleMapTest.Graph()
  // This creates Node and Factor objects which automatically add themselves to the outer Graph
  val shouldBeFalse = g.Node(nodeType, features = Map("feat1" -> 0.5, "feat2" -> 1.0))
  val shouldBeTrue = g.Node(nodeType, features = Map("feat1" -> 1.0, "feat2" -> 0.5))

  val map : Map[g.Node, String] = g.mapEstimate()
  println("should be false MAP value: "+map(shouldBeFalse))
  println("should be true MAP value: "+map(shouldBeTrue))
}

object BasicMarginal extends App {
  val basicMarginalExperiment = new GraphStream()
  val epsilonFactorType = basicMarginalExperiment.makeFactorType(2, weights =
    Map(
      List("false","true") -> Map("feat1" -> -1.0),
      List("true","false") -> Map("feat1" -> 1.0)
    )
  )

  // Create the graph

  val g = new basicMarginalExperiment.Graph()
  // This creates Node and Factor objects which automatically add themselves to the outer Graph
  val headNode = g.Node()
  val tailNode = g.Node()
  g.Factor(epsilonFactorType, List(headNode, tailNode), features = Map("feat1" -> 1.0))

  println(g.marginalEstimate())
  println(g.mapEstimate())
}

object EpsilonLearn extends App {

  ///////////////////////////////////////
  // An experiment to learn the value epsilon from random switchings
  ///////////////////////////////////////

  // Generate a set of booleans, that are either flipped or not

  val r : Random = new Random(42)
  val epsilon = r.nextFloat()
  val pairs = (0 to 1000).map(i => r.nextBoolean()).map(b => {
    // Keep the same
    if (r.nextFloat() > epsilon) {
      (b, b)
    }
    // Flip
    else {
      (b, !b)
    }
  }).toList

  // Proceed with actually using the API

  val learnEpsilonExperiment = new GraphStream()

  // Create the weight-sharing types

  val nodeType = learnEpsilonExperiment.makeNodeType(Set("true", "false"))
  val epsilonFactorType = learnEpsilonExperiment.makeFactorType(List(nodeType, nodeType))

  // Create the graphs, one for each pair

  val graphs = pairs.map(p => {
    new learnEpsilonExperiment.Graph {
      // This creates Node and Factor objects which automatically add themselves to the outer Graph
      val headNode = Node(nodeType, observedValue = p._1.toString)
      val tailNode = Node(nodeType, observedValue = p._2.toString)
      val f = Factor(epsilonFactorType, List(headNode, tailNode))
    }
  })

  val marg = graphs.toList(0).marginalEstimate()

  // Learn the factor weights for those connected to observed variables

  learnEpsilonExperiment.learn(graphs)
  println(epsilonFactorType.weights)
  println(nodeType.weights)
}
