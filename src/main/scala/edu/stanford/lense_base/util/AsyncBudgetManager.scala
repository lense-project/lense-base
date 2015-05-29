package edu.stanford.lense_base.util

/**
 * Created by keenon on 5/28/15.
 *
 * Manages the problem of asynchronous exploration of a state space in the face of budget constraints. Take out a
 * reservation on any money you might need, and return it if it turns out you don't need it
 */
class AsyncBudgetManager {

  // This is a safety stop, where runs can be given specific budgets that they are not intended to cross.
  var budget = 0.0
  var reserved = new java.util.IdentityHashMap[Any, Double]()
  var spent = 0.0

  val budgetLock = new Object()

  def addBudget(amount : Double) = budgetLock.synchronized {
    budget += amount
  }

  def tryReserveBudget(amount : Double, owner : Any) : Boolean = budgetLock.synchronized {
    if (reserved.getOrDefault(owner, 0.0) > 0.0) throw new IllegalStateException("The same reserver shouldn't take more than one reservation at a time")
    if (budget >= amount) {
      budget -= amount
      reserved.put(owner, amount)
      System.err.println("Reserving amount $"+amount)
      System.err.println("Left-over budget: "+budget)
      true
    }
    else {
      false
    }
  }

  def spendReservedBudget(amount : Double, owner : Any) : Unit = budgetLock.synchronized {
    if (amount == 0) {
      if (reserved.containsKey(owner)) {
        budget += reserved.get(owner)
        System.err.println("Reclaimed reserve $"+reserved.get(owner)+", budget remaining unclaimed: $"+budget)
        reserved.put(owner, 0.0)
      }
    }
    else {
      if (reserved.get(owner) >= amount) {
        val totalReserved = reserved.get(owner)
        val amountRemaining = totalReserved - amount
        budget += amountRemaining
        reserved.put(owner, 0.0)

        System.err.println("Spent $"+amount+", reclaimed $"+amountRemaining+", budget remaining unclaimed: $"+budget)
      }
      else {
        throw new IllegalStateException("Trying to spend budget that was never properly reserved")
      }
    }
  }

}
