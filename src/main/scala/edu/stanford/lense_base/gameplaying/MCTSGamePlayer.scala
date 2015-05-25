package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.{HumanDelayDistribution, HumanErrorDistribution}
import edu.stanford.lense_base.graph.{GraphNode, Graph}
import edu.stanford.lense_base.humancompute.HumanComputeUnit
import scala.collection.mutable
import scala.util.Random

/**
 * Created by keenon on 5/24/15.
 *
 * Implements fast, tree-parallel UCT MCTS with Double Progressive Widening to do state space search
 */
object MCTSGamePlayer extends GamePlayer {
  val r = new Random()

  override def getOptimalMove(state: GameState): GameMove = {
    // Get all the legal moves
    val legalTopLevelMoves = getAllLegalMoves(state, reserveRealBudget = true)

    println("Legal top-level moves: "+legalTopLevelMoves)

    val rootNode = TreeNode(StateSample(state), legalTopLevelMoves, this, null)

    for (i <- 0 to 1000) {
      runSample(rootNode, r)
    }

    rootNode.mostVisitedAction()
  }

  // Run a simulation, simultaneously constructing the tree as we go
  def runSample(rootNode : TreeNode, r : Random) : TreeNode = {
    var node = rootNode
    while (node.isNonTerminal) {
      node = node.treePolicyStep(r,
        engine.getHumanErrorDistribution,
        engine.getHumanDelayDistribution)

      node.visits += 1
    }
    node
  }
}

case class StateSample(gameState : GameState,
                       extraHypotheticalTime : Long = 0,
                       inFlightLandingTimes : Map[(GraphNode, HumanComputeUnit), Long] = Map(),
                       terminal : Boolean = false) {

  def calculateReward() : Double = {
    if (!terminal) throw new IllegalStateException("Shouldn't be asking for reward at an intermediate state!")

    // reward is just negative loss
    -gameState.loss(extraHypotheticalTime)
  }

  def sampleNextState(gameMove : GameMove,
                      r : Random,
                      humanErrorDistribution : HumanErrorDistribution,
                      humanDelayDistribution : HumanDelayDistribution) : StateSample = {
    gameMove match {
      case obs : MakeHumanObservation =>
        // This adds another request in flight, samples a time for the request to take, and adds it to the pile
        val nextState = gameState.getNextStateForInFlightRequest(obs.node, obs.hcu, null)
        val requestDelay = humanDelayDistribution.sampleDelay()
        val newInFlightLandingTimes = inFlightLandingTimes + ((obs.node, obs.hcu) -> (extraHypotheticalTime + requestDelay))
        StateSample(nextState, extraHypotheticalTime, newInFlightLandingTimes)

      case wait : Wait =>
        // This waits for the next request to return, pops it off, and registers the observation
        if (inFlightLandingTimes.size == 0) throw new IllegalStateException("Should never be Waiting if no requests in flight. Just turn in the guess.")
        val nextRequestToReturn = inFlightLandingTimes.minBy(_._2)
        val newInFlightLandingTimes = inFlightLandingTimes - nextRequestToReturn._1

        val beliefMarginals = gameState.marginals(nextRequestToReturn._1._1)
        val obs : String = humanErrorDistribution.sampleGivenMarginals(beliefMarginals)

        val nextState = gameState.getNextStateForNodeObservation(nextRequestToReturn._1._1, nextRequestToReturn._1._2, null, obs)
        StateSample(nextState, nextRequestToReturn._2, newInFlightLandingTimes)

      case turnIn : TurnInGuess =>
        // This returns a terminal state, which we can get loss from
        StateSample(gameState, extraHypotheticalTime, inFlightLandingTimes, terminal = true)
    }
  }
}

case class TreeNode(stateSample : StateSample, legalMoves : List[GameMove], gamePlayer : GamePlayer, parent : TreeNode) {
  // Mysterious constant for DPW
  // Basically when C > \sqrt{numChildrenForBestMove}, we explore at random, otherwise we sample from existing paths
  val C = 5.0

  val children = mutable.Map[GameMove, List[TreeNode]]()
  var visits = 0
  var totalObservedReward : Double = 0

  def fullyExpanded : Boolean = !legalMoves.exists(move => !children.contains(move))
  def isNonTerminal : Boolean = !stateSample.terminal

  def mostVisitedAction() : GameMove = {
    println(children.map(pair => pair._1.getClass.toString +": "+pair._2.map(_.visits).sum).mkString("\n"))
    children.maxBy(_._2.map(_.visits).sum)._1
  }

  def backpropReward(reward : Double) : Unit = {
    totalObservedReward += reward
    if (parent != null) parent.backpropReward(reward)
  }

  def backprop() = {
    if (isNonTerminal) throw new IllegalStateException("Shouldn't try to backprop from a non-terminal node")
    backpropReward(stateSample.calculateReward())
  }

  def treePolicyStep(r : Random,
                     humanErrorDistribution : HumanErrorDistribution,
                     humanDelayDistribution : HumanDelayDistribution) : TreeNode = {
    val unvisitedActions = legalMoves.filter(!children.contains(_))

    // First we use the UCT action maximizing score calculation to pick an action

    val bestMove = if (unvisitedActions.size > 0) {
      // If we have any unvisited actions, visit one

      // Always choose TurnInGuess() first, so that it can propagate the loss back up the tree quickly
      if (unvisitedActions.contains(TurnInGuess())) {
        TurnInGuess()
      }
      // Likewise with Wait(), so we can see how we're doing with the request we sent out already relatively quickly
      else if (unvisitedActions.contains(Wait())) {
        Wait()
      }
      // Otherwise visit in order of least certainty about
      else {
        unvisitedActions.head
      }
    } else {
      // Otherwise choose an action based on UCB1 multi-armed bandit formula
      legalMoves.maxBy(move => {
        val observedOutcomes = children(move)
        val sumRewardThisMove = observedOutcomes.map(_.totalObservedReward).sum
        val sumVisitsThisMove = observedOutcomes.map(_.visits).sum

        // Calculate score according to UCB1 formula
        (sumRewardThisMove / (sumVisitsThisMove + 1)) + Math.sqrt(2 * Math.log(visits) / (sumVisitsThisMove + 1))
      })
    }

    // Now that we have a move, use Double-Progressive-Widening to make sure we double-visit previously observed states
    // sometimes, but not too often

    val numChildrenForBestMove = if (children.contains(bestMove)) {
      children(bestMove).map(_.visits).sum
    } else {
      children.put(bestMove, List())
      0
    }

    if (numChildrenForBestMove == 0 || C > Math.sqrt(numChildrenForBestMove)) {
      // This means we get a new state
      val nextStateSample = stateSample.sampleNextState(bestMove, r, humanErrorDistribution, humanDelayDistribution)
      val nextTreeNode = TreeNode(nextStateSample, gamePlayer.getAllLegalMoves(nextStateSample.gameState, reserveRealBudget = false), gamePlayer, this)
      children.put(bestMove, children(bestMove) :+ nextTreeNode)
      nextTreeNode
    }
    else {
      // This means we pick a state uniformly from previously visited ones
      children(bestMove)(r.nextInt(children(bestMove).size))
    }
  }
}
