package edu.stanford.lense_base.humancompute

import edu.stanford.lense_base.server.WebWorkUnit

import scala.collection.mutable
import scala.util.Try

/**
 * Created by keenon on 5/5/15.
 *
 * Holds the state for a human compute unit, which can be sub classed to implement different kinds of state wrt oracles.
 *
 * HCU's are held in an HCU pool, which is how a system can decide who is available to query, and how long it will take
 * them to respond
 */
trait HumanComputeUnit {
  val workQueue = mutable.Queue[WorkUnit]()
  var currentWork : WorkUnit = null
  var startedWorkMillis : Long = 0

  def addWorkUnit(workUnit : WorkUnit) = {
    // Add work unit
    workQueue.synchronized {
      workQueue.enqueue(workUnit.asInstanceOf[WorkUnit])
      workQueue.notifyAll()
    }
  }

  def revokeWorkUnit(workUnit : WorkUnit) = {
    workUnit.revoke()
    workQueue.synchronized {
      workQueue.dequeueAll(_ eq workUnit)
    }
    if (workUnit eq currentWork) {
      cancelCurrentWork()
      currentWork = null
    }
  }

  def finishWork(workUnit : WorkUnit, answer : String) = {
    if (!workUnit.isRevoked) {
      workUnit.promise.complete(Try {
        answer
      })
    }

    workQueue.synchronized {
      if (workUnit == currentWork) {
        currentWork = null
      }
    }
  }

  def blockToPerformMoreWork() : Unit = {
    var workToPerform : WorkUnit = null
    while (true) {
      workQueue.synchronized {
        if (workQueue.isEmpty) {
          workQueue.wait()
        }
        else {
          workToPerform = workQueue.dequeue()
        }
      }
    }
    currentWork = workToPerform
    startedWorkMillis = System.currentTimeMillis()
    startWork(workToPerform)
  }

  def estimateTimeToFinishCurrentItem : Long = {
    if (currentWork == null) 0
    else {
      val elapsedTime = System.currentTimeMillis() - startedWorkMillis
      Math.max(0, estimateRequiredTimeToFinishItem(currentWork) - elapsedTime)
    }
  }

  def estimateTimeToFinishQueue : Long = {
    estimateTimeToFinishCurrentItem + workQueue.map(estimateRequiredTimeToFinishItem).sum
  }

  def estimateRequiredTimeIncludingQueue(workUnit : WorkUnit) : Long = {
    estimateRequiredTimeToFinishItem(workUnit) + estimateTimeToFinishQueue
  }

  // Gets the estimated required time to perform this task, in milliseconds
  def estimateRequiredTimeToFinishItem(workUnit : WorkUnit) : Long
  // Kick off a job
  def startWork(workUnit : WorkUnit)
  // Cancel the current job
  def cancelCurrentWork()
  // Get the cost
  def cost : Double
}
