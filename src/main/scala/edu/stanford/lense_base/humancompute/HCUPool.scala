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
      hcu.revokeAllWork()
      hcuPool.remove(hcu)
    }
  }

  def registerHCUArrivedCallback(obj : AnyRef, callback : () => Unit): Unit = {
    callbacks.put(obj,callback)
  }

  def removeHCUArrivedCallback(obj : AnyRef): Unit = {
    callbacks.remove(obj)
  }
}
