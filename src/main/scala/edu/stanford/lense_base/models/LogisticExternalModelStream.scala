package edu.stanford.lense_base.models

import edu.stanford.lense_base.humancompute.HumanErrorDistribution

/**
 * Created by keenon on 5/29/15.
 *
 * Implements a stupid logistic model over the data, which can be tweaked as desired
 */
abstract class LogisticExternalModelStream[Input](humanErrorDistribution : HumanErrorDistribution) extends UnivariateExternalModelStream[Input](humanErrorDistribution) {
  /**
   * Extract the features to use for logistic regression
   * @param input the input over which to extract features
   * @return
   */
  def getFeatures(input: Input): Map[String, Double]

  /**
   * Do prediction over the univariate output distribution
   * @param input the input object
   * @return
   */
  override def prior(input: Input): Map[String, Double] = Map()

  /**
   * Retrain the model. This can be a no-op if you want.
   *
   * @param inputOutputPairs a list of inputs to train against
   * @return the loss from model training
   */
  override def train(inputOutputPairs: Iterable[(Input, String)]): Double = 0.0
}
