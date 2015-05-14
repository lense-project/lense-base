package edu.stanford.lense_base.graph

import cc.factorie.model.{WeightsMap, WeightsSet}
import cc.factorie.optimize.AdaGrad

import scala.collection.mutable.ListBuffer

/**
 * Created by keenon on 5/10/15.
 */
class BatchAdaGrad() extends AdaGrad {
  var _isConverged = false
  override def isConverged = _isConverged

  var l2regularization = 0.2

  var lastValue = Double.NegativeInfinity
  var convergenceCounter = 0
  val valueHistory = ListBuffer[Double]()

  override def lRate(weights: WeightsSet, gradient: WeightsMap, value: Double): Double = {
    val regularizer = - (weights.dot(weights) * l2regularization / 2)
    val valuePlusRegularizer = value + regularizer
    System.err.println("Value: "+value)
    System.err.println("Regularizer: "+regularizer)
    System.err.println("Value + Regularizer: "+valuePlusRegularizer)

    if (lastValue != Double.NegativeInfinity) {
      val K = 10
      if (valueHistory.size > K) {
        var lastKAvg = 0.0
        var count = 0
        for (i <- valueHistory.size - K to valueHistory.size - 1) {
          lastKAvg += valueHistory(i)
          count += 1
        }
        lastKAvg /= count

        val percentageDifferenceFromFloatingValue = Math.abs(lastKAvg - valuePlusRegularizer) / Math.abs(lastKAvg)
        System.err.println("Percentage difference from avg of last "+K+": "+percentageDifferenceFromFloatingValue)
        if (valuePlusRegularizer > lastKAvg && percentageDifferenceFromFloatingValue < 0.01) {
          System.err.println("Converged to within %1 of avg of last " + K + " losses, quitting b/c oscillating")
          _isConverged = true
        }
      }

      val percentageImprovement = Math.abs(lastValue-valuePlusRegularizer)/Math.abs(lastValue)
      System.err.println("Value percentage improvment: "+percentageImprovement)
      if ((lastValue <= valuePlusRegularizer) && percentageImprovement < 0.005) {
        convergenceCounter += 1
        System.err.println("Convergence counter: "+convergenceCounter)
        if (convergenceCounter > 3) {
          _isConverged = true
        }
      }
      else {
        convergenceCounter = 0
      }
    }
    valueHistory.+=(valuePlusRegularizer)
    lastValue = valuePlusRegularizer

    super.lRate(weights, gradient, valuePlusRegularizer)
  }

  // We just override to put in our regularizer... muahahaha
  override def processGradient(weights: WeightsSet, gradient: WeightsMap): Unit = {
    gradient += (weights, -l2regularization)
    super.processGradient(weights, gradient)
  }
}
