package edu.stanford.lense_base.gameplaying

import scala.collection.mutable
import edu.stanford.lense_base.util.sample

/**
 * Created by chaganty on 5/24/15.
 * Handles MCTS exploration strategy
 * When using, create a MCTSTree and then sample it a couple 100 times.
 */
case class MCTSTree[State,Action,Label]() {
  abstract class TreeNode() {
    var visits = 0 // Count the number of times this node has been visited
    def value(state:State) : Double
  }
  case class Max(actions : Map[Action, TreeNode], visitFn : (State, Action) => State) extends TreeNode() {
    val values = mutable.Map(actions.mapValues(_ => 0.0).toSeq: _*)

    override def value(state: State) : Double = {
      visits += 1
      // Explore each node at least once
      val unvisited = actions filter {case (a, v) => actions.get(a).exists(_.visits == 0)}

      // Sample an action according to the UCT policy
      val (action, next) =
        if (unvisited.nonEmpty) {
           unvisited.find(_ => true).get
        } else {
          val (a, _) = mostPromisingChild()
          actions.find(_._1 == a).get
        }
      println("Chose " + action)
      // update state and pass it along.
      val reward = next.value(visitFn(state, action))

      // Update the value in the policy you are storing
      val (v0, n0) = (values.get(action).get, next.visits)
      values.update(action, v0 + (reward - v0)/n0)
      println ("Values + " + values)
      // Return the value of this action
      reward
    }

    def bestChild() : (Action, Double) = values.maxBy(_._2)
    // Get the UCT action
    def mostPromisingChild() : (Action, Double) = {
      println(values map { case (a, v) =>
        val Nv = actions.get(a).map(_.visits).getOrElse(1)
        v + Math.sqrt(2 * Math.log(visits) / Nv)
      })
      values maxBy { case (a, v) =>
        val Nv = actions.get(a).map(_.visits).getOrElse(1)
        v + Math.sqrt(2 * Math.log(visits) / Nv)
      }
    }
  }
  case class Expect(children : Map[Label, TreeNode], sampleFn : State => Map[Label, Double],
                    updateFn : (State, Label) => State) extends TreeNode() {
    // Sample a label, update state and pass it along.
    override def value(state: State): Double = {
      visits += 1
      // Sample a label to look at
      val lbl = sample(sampleFn(state))
      val next = children.get(lbl).get
      // update state and pass it along.
      next.value(updateFn(state, lbl))
    }
  }
  case class Leaf(valueFn : State => Double) extends TreeNode() {
    def value(state : State) = {
      visits += 1
      valueFn(state)
    }
  }

  def sampleBestAction(state : State, root : TreeNode, nSamples : Int = 100) : (Action, Double) = {
    Range(0, nSamples).foreach(_ => {
      root.value(state)
    })
    root.asInstanceOf[Max].bestChild()
  }
}
