package edu.stanford.lense_base.humancompute

import edu.stanford.lense_base.graph.GraphNode
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
  var running : Boolean = true

  def addWorkUnit(workUnit : WorkUnit) = {
    if (!running) {
      // If someone tries to add a task after we're dead, just kill the task immediately
      workUnit.taskFailed()
    }
    else {
      // Add work unit
      workQueue.synchronized {
        workQueue.enqueue(workUnit.asInstanceOf[WorkUnit])
        workQueue.notify()
      }
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
      workQueue.synchronized {
        workQueue.notify()
      }
    }
  }

  def revokeAllWorkAndKill() = {
    workQueue.synchronized {
      workQueue.map(_.revoke())
      workQueue.dequeueAll((unit) => true)
      if (currentWork != null) {
        currentWork.revoke()
        cancelCurrentWork()
        currentWork = null
      }
      running = false
      workQueue.notify()
    }
  }

  def finishWork(workUnit : WorkUnit, answer : String) = {
    if (workUnit != currentWork) {
      throw new IllegalStateException("Shouldn't be finishing something that isn't our current work")
    }
    if (!workUnit.isRevoked) {
      workUnit.finished(this)
      workUnit.promise.complete(Try {
        answer
      })
    }

    workQueue.synchronized {
      if (workUnit == currentWork) {
        currentWork = null
      }
      // Wake up the work performing code
      workQueue.notify()
    }
  }

  def estimateTimeToFinishCurrentItem : Long = {
    if (currentWork == null) 0
    else {
      val elapsedTime = System.currentTimeMillis() - startedWorkMillis
      Math.max(0, estimateRequiredTimeToFinishItem(currentWork.graphNode) - elapsedTime)
    }
  }

  def estimateTimeToFinishQueue : Long = {
    estimateTimeToFinishCurrentItem + workQueue.map(work => estimateRequiredTimeToFinishItem(work.graphNode)).sum
  }

  def estimateRequiredTimeIncludingQueue(node : GraphNode) : Long = {
    estimateRequiredTimeToFinishItem(node) + estimateTimeToFinishQueue
  }

  // Gets the estimated required time to perform this task, in milliseconds
  def estimateRequiredTimeToFinishItem(node : GraphNode) : Long
  // Kick off a job
  def startWork(workUnit : WorkUnit)
  // Cancel the current job
  def cancelCurrentWork()
  // Get the cost
  def cost : Double

  // Kick off computation
  new Thread {
    override def run() : Unit = {
      while (true) {
        if (!running) return

        var workToPerform: WorkUnit = null
        workQueue.synchronized {
          while (currentWork != null || workQueue.isEmpty) {
            workQueue.wait()
            if (!running) return
          }
          if (!running) return

          workToPerform = workQueue.dequeue()
          currentWork = workToPerform
          startedWorkMillis = System.currentTimeMillis()
          startWork(workToPerform)
        }
      }
    }
  }.start()
}
