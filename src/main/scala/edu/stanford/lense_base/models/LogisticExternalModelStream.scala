package edu.stanford.lense_base.models

import java.util

import edu.stanford.lense_base.humancompute.HumanErrorDistribution
import edu.stanford.nlp.classify._
import edu.stanford.nlp.ling.{BasicDatum, Datum}
import edu.stanford.nlp.stats.{ClassicCounter, Counter}
import edu.stanford.nlp.ling.RVFDatum

/**
 * Created by keenon on 5/29/15.
 *
 * Implements a stupid logistic model over the data, which can be tweaked as desired
 */
abstract class LogisticExternalModelStream[Input](humanErrorDistribution : HumanErrorDistribution, classes : List[String]) extends UnivariateExternalModelStream[Input](humanErrorDistribution) {
  var classifier : LinearClassifier[String, String] = null
  val classifierFactory : LinearClassifierFactory[String, String] = new LinearClassifierFactory[String,String]()

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
  override def prior(input: Input): Map[String, Double] = {
    if (classifier == null) {
      classes.map(cl => (cl, 1.0)).toMap
    }
    else {
      val counter = new ClassicCounter[String]()
      val feats = getFeatures(input)
      for (feat <- feats) {
        counter.incrementCount(feat._1, feat._2)
      }
      val rvf = new RVFDatum[String, String](counter)
      val outputCounter = classifier.probabilityOf(rvf)

      outputCounter.keySet().toArray.map(key => {
        (key.asInstanceOf[String], outputCounter.getCount(key))
      }).toMap
    }
  }

  /**
   * Retrain the model. This can be a no-op if you want.
   *
   * @param inputOutputPairs a list of inputs to train against
   * @return the loss from model training
   */
  override def train(inputOutputPairs: Iterable[(Input, String)]): Double = {
    if (inputOutputPairs.size == 0) return 0.0

    val rvfDataset : RVFDataset[String, String] = new RVFDataset[String,String]()

    inputOutputPairs.foreach(pair => {
      val counter = new ClassicCounter[String]()
      val feats = getFeatures(pair._1)
      for (feat <- feats) {
        counter.incrementCount(feat._1, feat._2)
      }

      val datum = new RVFDatum[String,String](counter, pair._2)
      rvfDataset.add(datum)
    })

    classifierFactory.synchronized {
      classifierFactory.useConjugateGradientAscent()
      // Turn on per-iteration convergence updates
      classifierFactory.setVerbose(true)
      // Large amount of smoothing
      classifierFactory.setSigma(0.3)
      // Build a classifier
      classifier = classifierFactory.trainClassifier(rvfDataset) // , null, new LogPrior()
      classifierFactory.notifyAll()
    }

    0.0
  }
}
