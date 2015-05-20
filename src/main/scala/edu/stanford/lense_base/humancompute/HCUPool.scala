package edu.stanford.lense_base.humancompute

import scala.collection.mutable

/**
 * Created by keenon on 5/5/15.
 *
 * Holds a dynamic set of human compute units, which can be added and removed at will
 */
class HCUPool {
  val hcuPool = mutable.HashSet[HumanComputeUnit]()
  val callbacks = mutable.HashMap[AnyRef, () => Unit]()

  var numWorkUnitsRequested = 0
  var numWorkUnitsCompleted = 0
  var numWorkUnitsRevoked = 0
  var totalMoneySpent = 0.0

  def addHCU(hcu : HumanComputeUnit): Unit = {
    hcuPool.synchronized {
      hcuPool.add(hcu)
    }

    for (callback <- callbacks) {
      callback._2.apply()
    }
  }

  def removeHCU(hcu : HumanComputeUnit): Unit = {
    hcuPool.synchronized {
      hcuPool.remove(hcu)
      // It's important that we not revoke all work until after we've removed this from the pool,
      // else the failed tasks can trigger a reassessment which may think this is still an active
      // HCU
      hcu.revokeAllWorkAndKill()
    }
  }

  def registerHCUArrivedCallback(obj : AnyRef, callback : () => Unit): Unit = {
    callbacks.put(obj,callback)
  }

  def removeHCUArrivedCallback(obj : AnyRef): Unit = {
    callbacks.remove(obj)
  }
}
