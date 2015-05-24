package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph.{GraphNode, Graph}
import scala.collection.mutable

/**
 * Created by keenon on 5/24/15.
 *
 * Implements fast, tree-parallel UCT MCTS with Double Progressive Widening to do state space search
 */
class MCTSGamePlayer extends GamePlayer {
  override def getOptimalMove(state: GameState): GameMove = {
    val legalTopLevelMoves = getAllLegalMoves(state, reserveRealBudget = true)
    val rootNode = TreeNode(state, legalTopLevelMoves)
    TurnInGuess()
  }

  // Find the next node to explore
  def treePolicy(rootNode : TreeNode) : TreeNode = {
    var node = rootNode
    while (node.isNonTerminal) {
      if (!node.fullyExpanded) {
        node = expand(node)
      }
      else {
        node = bestChild(node)
      }
    }
    rootNode
  }

  def expand(node : TreeNode) : TreeNode = {
    // TODO
    node
  }

  def bestChild(node : TreeNode) : TreeNode = {
    // TODO
    node
  }

  // Simulate to completion
  def defaultPolicy(node : TreeNode) : Double = {
    0.0
  }
}

case class TreeNode(gameState : GameState, legalMoves : List[GameMove]) {
  val children = mutable.Map[GameMove, TreeNode]()
  var visits = 0

  def fullyExpanded : Boolean = !legalMoves.exists(move => !children.contains(move))
  def isNonTerminal : Boolean = children.nonEmpty
}
