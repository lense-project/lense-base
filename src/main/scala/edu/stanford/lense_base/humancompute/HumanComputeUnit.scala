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
      println("Adding work: "+workUnit)
      println("Current queue: "+workQueue)
      workQueue.notify()
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
      // Wake up the work performing code
      workQueue.notify()
    }
  }

  // This assumes that you won't want to be performing more work
  def revokeAllWork() = {
    workQueue.map(_.revoke())
    workQueue.synchronized {
      workQueue.dequeueAll((unit) => true)
      if (currentWork != null) {
        cancelCurrentWork()
        currentWork = null
        // Wake up the work performing code
        workQueue.notify()
      }
    }
  }

  def finishWork(workUnit : WorkUnit, answer : String) = {
    if (workUnit != currentWork) {
      throw new IllegalStateException("Shouldn't be finishing something that isn't our current work")
    }
    if (!workUnit.isRevoked) {
      workUnit.promise.complete(Try {
        answer
      })
    }

    println("FINISHED WORK")

    workQueue.synchronized {
      if (workUnit == currentWork) {
        currentWork = null
      }
      // Wake up the work performing code
      workQueue.notify()
    }
  }

  def performWorkBlocking() : Unit = {
    while (true) {
      var workToPerform: WorkUnit = null
      workQueue.synchronized {
        while (currentWork != null || workQueue.isEmpty) {
          workQueue.wait()
        }

        workToPerform = workQueue.dequeue()
        println("Performing work: " + workToPerform)
        println("Current queue: " + workQueue)
        currentWork = workToPerform
        startedWorkMillis = System.currentTimeMillis()
        startWork(workToPerform)
      }
    }
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

  // Kick off computation
  new Thread {
    override def run() = {
      performWorkBlocking()
    }
  }.start()
}
