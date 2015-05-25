package edu.stanford.lense_base.gameplaying

import edu.stanford.lense_base.graph.{GraphNode, Graph}
import scala.collection.mutable

/**
 * Created by keenon on 5/24/15.
 *
 * Implements fast, tree-parallel UCT MCTS with Double Progressive Widening to do state space search
 */
class MCTSGamePlayer extends GamePlayer {

  def constructMCTSTree(graph: Graph, state:GameState, labels : List[String]) = {
    val t = MCTSTree[GameState, GraphNode, String]()
    import t._
    // Currently assuming that there is no game state
    // Do a one step lookahead
    val leaf = Leaf(_.loss(0))
    def expectNode(node : GraphNode) : Expect =
      // TODO: Update the state by saying that the measurement node was observed to be label
      Expect(labels.map(l => (l,leaf)).toMap, s => s.marginals.get(node).get, (s,l) => s)
    // Create a max node over possible actions to take == nodes in the graph
    Max(graph.nodes.map(node => (node, expectNode(node))).toMap,
      // TODO: Update state by adding a measurement node to the graph state.
      (s, node) => s)
  }

  override def getOptimalMove(state: GameState): GameMove = {
    // Get all the legal moves
    val legalTopLevelMoves = getAllLegalMoves(state, reserveRealBudget = true)
    // Turns out we don't really care about them though.
    // Instead, we find the best node to query (could be none of them).
    getOptimalNodeToQuery(state) match {
      case Some(node) => (legalTopLevelMoves find {
        case MakeHumanObservation(node_, _) => node_ == node
        case _ => false
      }).getOrElse(TurnInGuess())
      case None => TurnInGuess()
    }
  }

  // Find the optimal node to query; this is
  def getOptimalNodeToQuery(state: GameState) : Option[GraphNode] = {
    val null_utility = utility(state, None)
    val (query_utility, node) = state.originalGraph.nodes
      .map(node => (utility(state, Some(node)), node))
      .maxBy(_._1)
    if (null_utility > query_utility) None else Some(node)
  }

  // Gets the utility for querying a particular node.
  def utility(state:GameState, node: Option[GraphNode]) : Double = {
    0.0
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

//  def getNextStates(move : GameMove) : List[(Double,GameState)] = {
//    // could perform some cacheing here, to help prevent the need for explicit Dynamic Programming elsewhere
//    move match {
//      case obs : MakeHumanObservation => {
//        obs.node.nodeType.possibleValues.map(randomHumanResponse => {
//          (marginals(oldToNew(obs.node))(randomHumanResponse),getNextStateForNodeObservation(obs.node, obs.hcu, null, randomHumanResponse))
//        })
//      }.toList
//      case _ : TurnInGuess => List()
//    }
//  }
}

case class TreeNode(gameState : GameState, legalMoves : List[GameMove]) {
  val children = mutable.Map[GameMove, TreeNode]()
  var visits = 0

  def fullyExpanded : Boolean = !legalMoves.exists(move => !children.contains(move))
  def isNonTerminal : Boolean = children.nonEmpty
}
