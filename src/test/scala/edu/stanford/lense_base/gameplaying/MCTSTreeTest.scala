package edu.stanford.lense_base.gameplaying

import org.scalatest.FunSuite

import scala.util.Random

/**
 * Created by chaganty on 5/24/15.
 */
class MCTSTreeTest extends FunSuite {

  test("MCTS tree finds the right action in a non-stochastic stateless setting") {
    val N = 100
    val tree = MCTSTree[Char, Char, Char]()
    import tree._
    val root = Max(Map('a' -> tree.Leaf(_ => 1.0), 'b' -> tree.Leaf(_ => 2.0)), (s,a) => s)
    val (action, score) = tree.sampleBestAction('a', root, N)
    assert (action == 'b')
    assert (score == 2.0)
    val visits = root.actions.mapValues(_.visits)
    println(visits)
    assert (visits.get('a').get > 1)
    assert (visits.get('b').get > .9 * N)
  }

  test("MCTS tree finds the right action in a stochastic stateless setting") {
    Random.setSeed(0)
    val N = 300
    val tree = MCTSTree[Char, Char, Char]()
    import tree._
    val root = Max(Map('a' -> tree.Leaf(_ => if (Random.nextDouble > 0.9) 10.0 else 0.0), 'b' -> tree.Leaf(_ => if (Random.nextDouble > 0.5) 1.0 else 0.0)), (s,a) => s)
    val (action, score) = tree.sampleBestAction('a', root, N)
    val visits = root.actions.mapValues(_.visits)

    println("Best action: " + action)
    println("Values of actions: " + root.values)
    println("Visits of actions: " + visits)

    assert (action == 'a')
  }

  test("MCTS tree finds the right action in a stochastic tranisition setting") {
    Random.setSeed(0)
    val N = 300
    val tree = MCTSTree[Char, Char, Char]()
    import tree._

    val rewards = Map('a' -> 1.0, 'b' -> 0.0)
    val leaf = Leaf(rewards.get(_).get)
    val kids = Map('a' -> leaf, 'b' -> leaf)

    val root = Max(Map(
      'a' -> Expect(kids, _ => Map('a' -> 0.8, 'b' -> 0.2), (_,l) => l ),
      'b' -> Expect(kids, _ => Map('a' -> 0.5, 'b' -> 0.5), (_,l) => l )),
      (_, a) => a
    )
    val (action, score) = tree.sampleBestAction('a', root, N)
    val visits = root.actions.mapValues(_.visits)

    println("Best action: " + action)
    println("Values of actions: " + root.values)
    println("Visits of actions: " + visits)

    assert (action == 'a')
    assert (root.values.get('a').get === (0.8 +- 0.05))
    assert (root.values.get('b').get === (0.5 +- 0.05))
  }

}
