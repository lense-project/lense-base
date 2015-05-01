package edu.stanford.lense_base

import edu.stanford.lense_base.examples.{NERExample, LenseFramework}
import edu.stanford.lense_base.gameplaying.LookaheadOneHeuristic

/**
 * Created by keenon on 4/30/15.
 *
 * This gets called by the server when it's ready to receive queries from humans
 */
object Init {

  def main(args : Array[String]): Unit = {
    NERExample.testWithRealHumans()
  }
}
