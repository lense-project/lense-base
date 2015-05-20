package edu.stanford.lense_base.humancompute

import edu.stanford.lense_base.graph.GraphNode

import scala.concurrent.Promise
import scala.util.Try

/**
 * Created by keenon on 5/5/15.
 *
 * Holds a single job to ask an HCU pool to complete
 */
class WorkUnit(resultPromise : Promise[String], initNode : GraphNode, hcuPool : HCUPool) {

  val graphNode = initNode
  private var _revoked = false

  hcuPool.numWorkUnitsRequested += 1

  def revoke() = this.synchronized {
    _revoked = true
    hcuPool.numWorkUnitsRevoked += 1
    taskFailed()
  }

  def isRevoked : Boolean = this.synchronized {
    _revoked
  }

  def taskFailed(e : Throwable = new WorkNotCompletedException()) : Unit = {
    resultPromise.complete(Try {
      throw e
    })
  }

  def promise = resultPromise

  def finished(finisher : HumanComputeUnit) = {
    hcuPool.numWorkUnitsCompleted += 1
    hcuPool.totalMoneySpent += finisher.cost
  }
}

class WorkNotCompletedException extends Exception
