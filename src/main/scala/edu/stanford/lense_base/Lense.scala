package edu.stanford.lense_base

import edu.stanford.lense_base.graph._

/**
 * Created by keenon on 4/27/15.
 *
 * This is the central static dispatcher to handle requests to the API
 */
object Lense {
  def predict(graph : Graph, askHuman : GraphNode => String) : Map[GraphNode, String] = {
    graph.nodes.map(n => {
      (n, askHuman(n))
    }).toMap
  }
}
