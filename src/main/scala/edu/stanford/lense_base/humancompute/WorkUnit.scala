package edu.stanford.lense_base.humancompute

import scala.concurrent.Promise
import scala.util.Try

/**
 * Created by keenon on 5/5/15.
 *
 * Holds a single job to ask an HCU pool to complete
 */
class WorkUnit(resultPromise : Promise[String]) {

  private var _revoked = false

  def revoke() = this.synchronized {
    _revoked = true
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
}

class WorkNotCompletedException extends Exception
