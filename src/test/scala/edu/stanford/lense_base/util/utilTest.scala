package edu.stanford.lense_base.util

import org.scalatest.FunSuite
import edu.stanford.lense_base.util.sample

/**
 * Created by chaganty on 5/24/15.
 */
class utilTest extends FunSuite {

  test("Sample generates the right distribution") {
    val N = 1000
    val pdf = Map('a' -> 0.3, 'b' -> 0.5, 'c' -> 0.2)
    val cnts = Range(0, N).map(_ => sample(pdf)).groupBy(x => x).mapValues(_.length.toFloat / N.toFloat)
    println(pdf)
    println(cnts)
    pdf.keys.foreach(k =>
      assert(Math.abs(pdf.get(k).get - cnts.get(k).get) < 0.05))
  }

}
