package edu.stanford.lense_base.model

import edu.stanford.lense_base.humancompute.{HumanComputeUnit, HumanErrorDistribution}

/**
 * Created by keenon on 5/28/15.
 *
 * This abstracts over multiple different kinds of model weight-sharing systems, and is basically only responsible for
 * being able to do learning.
 */
abstract class ModelStream(humanErrorDistribution : HumanErrorDistribution) {
  /**
   * Retrains the model based on all examples seen so far.
   * @return model loss, for tracking and debugging optimizers
   */
  def learn(models : Iterable[Model]) : Double

  /**
   * Gets a new model (an empty graph, or a single logit with no features) from this modelStream, which can then be
   * manipulated and specialized according to the subclass type.
   *
   * @return a new model
   */
  def newModel() : Model
}

/**
 * This abstracts over multiple kinds of Models, like GraphicalModel and LogisticModel, with different internal
 * behavior. The key reason you can't conveniently do everything in GraphicalModel is that tuning is a pain, so we have
 * to go to custom implementations for univariate cases that let us do a much better job tuning.
 */
abstract class Model(modelStream : ModelStream) {

  /**
   * Adds a new observation to the given variable, with updates according to the modelStream.humanErrorDistribution,
   * and returns the new model representing this new state of the world.
   *
   * @param variable the variable to which we will add an observation
   * @param observation the value observed
   * @return a new model, representing adding this observation
   */
  def cloneModelWithHumanObservation(variable : ModelVariable, observation : String) : Model

  /**
   * @return a map of the variables -> distributions over tokens
   */
  def marginals : Map[ModelVariable, Map[String,Double]]

  /**
   * @return a map of variables -> MAP assignment
   */
  def map : Map[ModelVariable, String]

  /**
   * @return a list of the variables that this model covers
   */
  def variables : List[ModelVariable]
}

/**
 * These are basically just keys against internal state.
 *
 * They do expose one simple function that NodeType used to handle, possibleValues
 */
abstract class ModelVariable(m : Model) {
  def possibleValues : List[String]

  var observedValue : String = null

  def model : Model = m

  def setObservedValue(value : String) = {
    observedValue = value
  }

  def isObserved : Boolean = observedValue != null

  def getObservedValue : String = {
    if (!isObserved) throw new IllegalStateException("Can't get an observed value that was never set")
    observedValue
  }

  // A quick and dirty way to attach arbitrary state. Who said type systems are always good?
  def payload : Any
}
