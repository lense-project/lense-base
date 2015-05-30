package edu.stanford.lense_base.humancompute

import scala.io.Source
import scala.util.Random

/**
 * Created by keenon on 5/23/15.
 */
abstract class HumanDelayDistribution {
  def sampleDelay() : Int
}

case class ConstantHumanDelayDistribution(delay : Int) extends HumanDelayDistribution {
  override def sampleDelay(): Int = delay
}

case class ObservedHumanDelayDistribution(path : String, rand : Random) extends HumanDelayDistribution {
  lazy val list : List[Int] = {
    Source.fromFile(path).getLines().map(line => java.lang.Double.parseDouble(line).asInstanceOf[Int]).toList
  }
  // Just sample at random from a list of observed human delays
  override def sampleDelay(): Int = list(rand.nextInt(list.size))
}

case class ClippedGaussianHumanDelayDistribution(mean : Int, std : Int, rand : Random) extends HumanDelayDistribution {
  override def sampleDelay(): Int = mean + Math.round(rand.nextGaussian() * std).asInstanceOf[Int]
}

