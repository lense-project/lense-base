package edu.stanford.mydo

import edu.stanford.lense_base.LenseMulticlassUseCase
import edu.stanford.lense_base.humancompute.{EpsilonRandomErrorDistribution, ClippedGaussianHumanDelayDistribution, HumanDelayDistribution, HumanErrorDistribution}
import edu.stanford.lense_base.models.{LogisticExternalModelStream, ModelStream, Model, ModelVariable}
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConversions._
import scala.util.Random

/**
 * Created by keenon on 6/8/15.
 *
 * Implements a use-case for LENSE that communicates with email parsing
 */
class EmailUseCase extends LenseMulticlassUseCase[(Email, Int)] {
  override def labelTypes: List[String] = List("YES", "NO")

  override def getHumanQuestion(input: (Email, Int)): String = {
    "Does this sentence look like we should add it to a todo-list?<br>"+
    input._1.sentences.zipWithIndex.map(p => {
      if (p._2 == input._2) {
        "<span class='focus'>"+p._1.toString+"</span>"
      }
      else p._1.toString
    })
  }

  /**
   * This gets the initial training examples to show to humans. Provide an input, the correct answer, and
   * any comments in HTML that you want displayed while the user is doing this example.
   *
   * @return
   */
  override def getHumanTrainingExamples: List[((Email, Int), String, String)] = List()

  override def getHumanVersionOfLabel(label: String): String = label match {
    case "YES" => "Yes"
    case "NO" => "No"
  }

  override def getModelStream: ModelStream = new LogisticExternalModelStream[(Email, Int)](humanErrorDistribution, labelTypes) {
    /**
     * Extract the features to use for logistic regression
     * @param input the input over which to extract features
     * @return
     */
    override def getFeatures(input: (Email, Int)): Map[String, Double] = {
      val coremap : CoreMap = input._1.sentences(input._2)

      coremap.get(classOf[TokensAnnotation]).toList.map(token => {
        token.lemma() -> 1.0
      }).toMap
    }

    /**
     * Defines the possible output values of the model
     * @return
     */
    override def possibleValues: List[String] = labelTypes
  }

  lazy val rand = new Random(42)
  override def humanDelayDistribution: HumanDelayDistribution = ClippedGaussianHumanDelayDistribution(2000, 2000, rand)
  override def humanErrorDistribution: HumanErrorDistribution = EpsilonRandomErrorDistribution(0.3, rand)

  /**
   * A way to define the loss function for you system. mostLikelyGuesses is a list of all the nodes being chosen on,
   * with their corresponding most likely label, and the probability the model assigns to the label.
   *
   * TODO: more docs here
   *
   * @param mostLikelyGuesses
   * @param cost
   * @param ms
   * @return
   */
  override def lossFunction(mostLikelyGuesses: List[(ModelVariable, String, Double)], cost: Double, ms: Long): Double = {
    (1.0 - mostLikelyGuesses.head._3) + cost
  }

  /**
   * Some gameplayers care about losses being expressed as reward (negative loss) in the [0,1] range. To accomplish this,
   * we need to know a max loss per node
   * @return
   */
  override def maxLossPerNode: Double = 2.0

  /**
   * This specifies the budget that this run will spend, in dollars. You may not use all of it, but the engine will stop
   * asking humans for help, and revert to simple machine learning, after it has exhausted the budget.
   *
   * @return amount in dollars to use as budget
   */
  override def budget: Double = 400.0

  override def getContextForHumanErrorReplay(variable: ModelVariable, model: Model): String = "N/A"
}
