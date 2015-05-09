package edu.stanford.lense_base.examples

import edu.stanford.lense_base.LenseMulticlassUseCase
import edu.stanford.lense_base.graph.GraphNode

/**
 * Created by keenon on 5/8/15.
 *
 * Simple synthetic example of 2 gaussians used to generate lots of synthetic data, then train
 */
class TwoGaussiansCase extends LenseMulticlassUseCase[(Double,Double)] {
  override def labelTypes: Set[String] = Set("A","B")

  override def getHumanQuestion(input: (Double, Double)): String = throw new UnsupportedOperationException("Don't run the twoGaussians dataset on real humans!")
  override def getHumanVersionOfLabel(label: String): String = throw new UnsupportedOperationException("Don't run the twoGaussians dataset on real humans!")

  override def getFeatures(input: (Double, Double)): Map[String, Double] = Map(
    "x" -> input._1,
    "y" -> input._2,
    "x+y" -> (input._1+input._2),
    "x-y" -> (input._1-input._2)
  )

  override def lossFunction(mostLikelyGuesses: List[(GraphNode, String, Double)], cost: Double, ms: Long): Double = ???
}
