package edu.stanford.lense_base.models

import edu.stanford.lense_base.humancompute.HumanErrorDistribution

/**
 * Created by keenon on 5/29/15.
 *
 * Shortcuts some of the redundancy for doing single variable prediction with an external model, which is a common case
 */
abstract class UnivariateExternalModelStream[Input](humanErrorDistribution : HumanErrorDistribution) extends ExternalModelStream[Input](humanErrorDistribution) {
  /**
   * Defines the possible output values of the model
   * @return
   */
  def possibleValues : List[String]

  /**
   * Do prediction over the univariate output distribution
   * @param input the input object
   * @return
   */
  def prior(input : Input) : Map[String, Double]

  /**
   * Retrain the model. This can be a no-op if you want.
   *
   * @param inputOutputPairs a list of inputs to train against
   * @return the loss from model training
   */
  def train(inputOutputPairs : Iterable[(Input, String)]): Double

  def devPairs : List[(Input, String)] = List()

  override def devSet : List[(ExternalModel[Input], Map[ExternalModelVariable, String])] = devPairs.map(pair => {
    val m = newModelForInput(pair._1)
    val key = Map(m.variables.head.asInstanceOf[ExternalModelVariable] -> pair._2)
    (m, key)
  })

  /**
   * Creates a new model in this model stream for a given input
   *
   * @param input the input to use
   * @return the model
   */
  def newModelForInput(input : Input) : ExternalModel[Input] = {
    val model = newModel().asInstanceOf[ExternalModel[Input]]
    val variable = List(new ExternalModelVariable(model, possibleValues, payload = input))
    model.setVariables(variable)
    model.setInput(input)
    model
  }

  /**
   * This does prediction to get a prior on a single input instance, using the model
   * @param externalModel the input object about which we're making predictions
   * @return a raw distribution over possible tags - don't worry about tuning, we'll handle that
   */
  override def rawPrior(externalModel: ExternalModel[Input]): Map[ExternalModelVariable, Map[String, Double]] = {
    Map(externalModel.variables.head.asInstanceOf[ExternalModelVariable] -> prior(externalModel.getInput))
  }

  /**
   * Trains the external model using a set of examples. This can be a no-op if the model is already well trained on some
   * dataset, and we don't want online training
   * @param inputOutputPairs the pairs of (input, output) to use in training, if desired
   */
  override def trainExternalModel(inputOutputPairs: Iterable[(Input, Map[ModelVariable, String])]): Double = {
    System.out.println("Training external model...")
    train(inputOutputPairs.map(pair => {
      (pair._1, pair._2.head._2)
    }))
  }
}
