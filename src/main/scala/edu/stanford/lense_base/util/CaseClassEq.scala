package edu.stanford.lense_base.util

/**
 * Created by keenon on 4/24/15.
 */
trait CaseClassEq {
  override def equals(o : Any) : Boolean = {
    o match {
      case a : AnyRef => this eq a
      case _ => false
    }
  }
}
