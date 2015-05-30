package edu.stanford.lense_base.mturk

import edu.stanford.lense_base.server.WorkUnitServlet

/**
 * Created by keenon on 5/18/15.
 *
 * This is a simple program to print all HITs for people it finds in the database, who have done some reasonable
 * amount of work and not been compensated yet. This lets you recover from crashes by paying people manually
 */
object ViewDroppedHITs {
  def main(args : Array[String]) : Unit = {
    for (worker <- MTurkDatabase.getAllWorkers) {
      if (!worker.currentlyConnected) {
        println(worker)
      }
    }
  }
}
