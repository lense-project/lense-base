package edu.stanford.lense_base

import scala.collection.mutable
import scala.util.Random

/**
 * Created by chaganty on 5/24/15.
 */
package object util {
  def sample[A](pdf : Map[A, Double]) : A = {
    // Construct a CDF
    val cdf = pdf.foldLeft(List[(Double,A)]()) {case (vs : List[(Double,A)], (l:A, v:Double)) =>
      if (vs.nonEmpty) (vs.head._1 + v, l) :: vs else (v, l) :: vs
    }.reverse
    // Iterate through the list and return the element
    val pivot = Random.nextDouble()
    cdf.find(_._1 > pivot).get._2
  }
}

