package edu.stanford.lense_base

import scala.io.Source
import scala.util.Random

/**
 * Created by keenon on 5/23/15.
 */
abstract class HumanErrorDistribution(rand : Random) {
  def guess(correct : String, possibleValues : Set[String]) : String
  def jointProbability(correct : String, guess : String) : Double

  def sampleGivenMarginals(marginals : Map[String,Double]) : String = {
    val possibleValues = marginals.map(_._1).toList

    val multipliedMarginals = possibleValues.map(guessedValue => {
      (guessedValue, marginals.map(pair => {
        val possibleTrueValue = pair._1
        val trueValueWithProb = pair._2
        jointProbability(possibleTrueValue, guessedValue) * trueValueWithProb
      }).sum)
    })

    var sample = rand.nextDouble()

    for (pair <- multipliedMarginals) {
      if (sample < pair._2) return pair._1
      sample -= pair._2
    }

    multipliedMarginals.last._1
  }
}

case class EpsilonRandomErrorDistribution(epsilon : Double, rand : Random) extends HumanErrorDistribution(rand) {
  override def guess(correct : String, possibleValues : Set[String]) : String = {
    // Do the automatic error generation
    if (rand.nextDouble() > epsilon) {
      correct
    }
    else {
      // Pick uniformly at random
      possibleValues.toList(rand.nextInt(possibleValues.size))
    }
  }

  override def jointProbability(correct: String, guess: String): Double = if (correct == guess) {
    1 - epsilon
  }
  else {
    epsilon
  }
}

case class ConfusionMatrixErrorDistribution(path : String, rand : Random) extends HumanErrorDistribution(rand) {
  lazy val unnormalizedConfusionMatrix : Map[String, List[(String,Double)]] = {
    val lines = Source.fromFile(path).getLines().toList
    val tags = lines(0).split(",")
    val map = lines.slice(1,lines.size).map(line => {
      val parts = line.split(",")
      val header = parts(0)
      val sections = parts.slice(1, parts.size).zipWithIndex.map(pair => {
        val num = Integer.parseInt(pair._1).toDouble
        val correspondsTo = tags(pair._2+1)
        (correspondsTo, num)
      })
      (header, sections.toList)
    }).toMap
    println("Retrieved confusion map: "+map)
    map
  }

  lazy val conditionalConfusionMatrix : Map[String, List[(String,Double)]] = {
    unnormalizedConfusionMatrix.map(pair => {
      (pair._1, {
        val sum = pair._2.map(_._2).sum
        pair._2.map(p => (p._1, p._2 / sum))
      })
    })
  }

  lazy val oneNormConfusionMatrix : Map[String, List[(String,Double)]] = {
    val sum = unnormalizedConfusionMatrix.flatMap(_._2).map(_._2).sum
    unnormalizedConfusionMatrix.map(pair => {
      (pair._1, pair._2.map(p => (p._1, p._2 / sum)))
    })
  }

  override def guess(correct: String, possibleValues: Set[String]): String = {
    val observedDistribution : List[(String,Double)] = conditionalConfusionMatrix(correct)
    var draw = rand.nextDouble()
    for (pair <- observedDistribution) {
      if (draw <= pair._2) {
        return pair._1
      }
      else {
        draw -= pair._2
      }
    }
    observedDistribution(observedDistribution.size - 1)._1
  }

  override def jointProbability(correct: String, guess: String): Double = {
    oneNormConfusionMatrix(correct).toMap.apply(guess)
  }
}

