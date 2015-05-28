package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.{HumanDelayDistribution, HumanErrorDistribution}
import edu.stanford.lense_base.graph.{GraphNode, Graph}
import edu.stanford.lense_base.humancompute.HumanComputeUnit
import scala.collection.mutable
import scala.concurrent.Lock
import scala.util.Random

/**
 * Created by keenon on 5/24/15.
 *
 * Implements fast, tree-parallel UCT MCTS with Double Progressive Widening to do state space search
 */
object MCTSGamePlayer extends GamePlayer {
  val r = new Random()

  val mainTreeLock = new Object()

  override def getOptimalMove(state: GameState): GameMove = {
    // Get all the legal moves
    val legalTopLevelMoves = getAllLegalMoves(state, reserveRealBudget = true)

    // println("Legal top-level moves: "+legalTopLevelMoves)

    val rootNode = TreeNode(StateSample(state), legalTopLevelMoves, this, null)

    val samplesToTake = 20*legalTopLevelMoves.size
    // val threadCount = Runtime.getRuntime.availableProcessors()
    val threadCount = 1

    val threads = (1 to threadCount).map(i => {
      new Thread(new Runnable {
        override def run(): Unit = {
          for (i <- 1 to samplesToTake / threadCount) {
            runSample(rootNode, r)
          }
        }
      })
    })

    threads.foreach(_.start())
    threads.foreach(_.join())

    /*
    println("Final tree:")
    println(rootNode.recursiveToString(0))
    */

    rootNode.mostVisitedAction()
  }

  // Run a simulation, simultaneously constructing the tree as we go
  def runSample(rootNode : TreeNode, r : Random) : Unit = {
    var holdLock = false

    holdLock = true

    var node = rootNode
    node.visits += 1

    while (node.isNonTerminal) {
      node = node.treePolicyStep(r,
        engine.getHumanErrorDistribution,
        engine.getHumanDelayDistribution)
      // We've just stepped off-tree, can hand back main lock
      if (node.visits == 0) {
        holdLock = false
      }
      node.visits += 1
    }

    mainTreeLock.synchronized {
      node.backprop()
    }
  }
}

case class StateSample(gameState : GameState,
                       startTime : Long = System.currentTimeMillis(),
                       hypotheticalEndTime : Long = System.currentTimeMillis(),
                       terminal : Boolean = false) {

  def calculateReward() : Double = {
    if (!terminal) throw new IllegalStateException("Shouldn't be asking for reward at an intermediate state!")

    // reward is just negative loss. If we go over the maxLossPerNode term, this will return negative, and MCTS just won't
    // visit this node ever again
    val normalizedLoss = gameState.loss(hypotheticalEndTime - startTime) / (gameState.maxLossPerNode * gameState.originalGraph.nodes.size)
    1.0 - normalizedLoss
  }

  def sampleNextState(gameMove : GameMove,
                      r : Random,
                      humanErrorDistribution : HumanErrorDistribution,
                      humanDelayDistribution : HumanDelayDistribution) : StateSample = {
    gameMove match {
      case obs : MakeHumanObservation =>
        // This adds another request in flight, samples a time for the request to take, and adds it to the pile
        val requestDelay = humanDelayDistribution.sampleDelay()
        val nextState = gameState.getNextStateForInFlightRequest(obs.node, obs.hcu, null, hypotheticalEndTime + requestDelay)
        StateSample(nextState, startTime, hypotheticalEndTime)

      case wait : Wait =>
        val inFlightLandingTimes = gameState.inFlightRequests.map(quad => {
          (quad._1, quad._2) -> (quad._4 + humanDelayDistribution.sampleDelay())
        })

        // This waits for the next request to return, pops it off, and registers the observation
        val nextRequestToReturn = inFlightLandingTimes.minBy(_._2)

        val beliefMarginals = gameState.marginals(nextRequestToReturn._1._1)
        val obs : String = humanErrorDistribution.sampleGivenMarginals(beliefMarginals)

        val nextState = gameState.getNextStateForNodeObservation(nextRequestToReturn._1._1, nextRequestToReturn._1._2, null, obs)
        StateSample(nextState, startTime, nextRequestToReturn._2)

      case turnIn : TurnInGuess =>
        // This returns a terminal state, which we can get loss from
        StateSample(gameState, hypotheticalEndTime, terminal = true)
    }
  }
}

case class TreeNode(stateSample : StateSample, legalMoves : List[GameMove], gamePlayer : GamePlayer, parent : TreeNode) {
  // Mysterious constant for DPW
  // Basically when C > \sqrt{numChildrenForBestMove}, we explore at random, otherwise we sample from existing paths
  val exploreChildren = 100
  val C = Math.sqrt(exploreChildren)
  // Myserious constant for UCT - UCB1, set to lower to explore less. Default 2.0
  val K = 2.0

  val children = mutable.Map[GameMove, List[TreeNode]]()
  var visits = 0
  var totalObservedReward : Double = 0

  def recursiveToString(level : Int): String = {
    "\t"*level+"{Node ["+
    "visits:"+visits+","+
    "delay:"+(stateSample.hypotheticalEndTime-stateSample.startTime)+","+
    "cost:$"+stateSample.gameState.cost+","+
    "totalObservedReward:"+totalObservedReward+","+
    "averageObservedReward:"+(totalObservedReward/visits)+"]"+
    children.map(pair => {
      "\n"+"\t"*(level+1)+pair._1+" outcomes = [\n"+
      pair._2.map(_.recursiveToString(level+2)).mkString(",\n")+
      "\n"+"\t"*(level+1)+"]"
    }).mkString("")+
    "\n"+"\t"*level+"}"
  }

  def isNonTerminal : Boolean = !stateSample.terminal

  def mostVisitedAction() : GameMove = {
    println(children.map(pair => pair._1+": "+pair._2.map(_.visits).sum+", reward "+(pair._2.map(_.totalObservedReward).sum / pair._2.map(_.visits).sum)).mkString("\n"))

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
    val unvisitedMoves = legalMoves.filter(!children.contains(_))

    // First we use the UCT action maximizing score calculation to pick an action

    val bestMove = if (unvisitedMoves.size > 0) {
      // If we have any unvisited actions, visit one

      // Always choose TurnInGuess() first, so that it can propagate the loss back up the tree quickly
      if (unvisitedMoves.contains(TurnInGuess())) {
        TurnInGuess()
      }
      // Likewise with Wait(), so we can see how we're doing with the request we sent out already relatively quickly
      else if (unvisitedMoves.contains(Wait())) {
        Wait()
      }
      // Otherwise visit in order of least certainty about
      else {
        unvisitedMoves.head
      }
    } else {
      // Otherwise choose an action based on UCB1 multi-armed bandit formula
      legalMoves.maxBy(move => {
        val observedOutcomes = children(move)
        // println("Size of children:"+children.size)

        val sumRewardThisMove = observedOutcomes.map(_.totalObservedReward).sum
        val sumVisitsThisMove = observedOutcomes.map(_.visits).sum

        // Calculate score according to UCB1 formula
        val score = (sumRewardThisMove / (sumVisitsThisMove + 1)) + Math.sqrt(K * Math.log(visits) / (sumVisitsThisMove + 1))
        // println("Score for "+move+" with this node visits "+visits+", move size:"+observedOutcomes.size+", moveVisits "+sumVisitsThisMove+", moveReward "+sumRewardThisMove+": "+score)
        score
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

    // TurnInGuess() is deterministic, so we don't need to handle infinite branching, cause all branches are the same
    val moveSupportsBranching : Boolean = bestMove match {
      case _ : TurnInGuess => false
      case _ : MakeHumanObservation => false
      case _ : Wait => true
    }

    if (numChildrenForBestMove == 0 || (C > Math.sqrt(numChildrenForBestMove) && moveSupportsBranching)) {
      if (children.contains(bestMove) && children(bestMove).size > 0) {
        // This means we pick a state uniformly from previously visited ones
        children(bestMove)(r.nextInt(children(bestMove).size))
      }
      else {
        // This means we get a new state
        val nextStateSample = stateSample.sampleNextState(bestMove, r, humanErrorDistribution, humanDelayDistribution)
        val nextTreeNode = TreeNode(nextStateSample, gamePlayer.getAllLegalMoves(nextStateSample.gameState, reserveRealBudget = false), gamePlayer, this)
        children.put(bestMove, children(bestMove) :+ nextTreeNode)
        nextTreeNode
      }
    }
    else {
      // This means we pick a state uniformly from previously visited ones
      children(bestMove)(r.nextInt(children(bestMove).size))
    }
  }
}
