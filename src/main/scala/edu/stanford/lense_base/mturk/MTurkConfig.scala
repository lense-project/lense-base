package edu.stanford.lense_base.mturk

/**
 * Created by keenon on 6/2/15.
 *
 * In liu of a YAML parser... cause meh
 */
object MTurkConfig {
  val retainerMinutes : Int = 45
  val turkerCost : Double = 0.01
  val timeoutSeconds : Int = 60*60
  val retainerCost : Double = 5.00
}
