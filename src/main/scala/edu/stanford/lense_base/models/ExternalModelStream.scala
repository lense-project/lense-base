package edu.stanford.lense_base.models

import edu.stanford.lense_base.humancompute.HumanErrorDistribution
import edu.stanford.lense_base.util.CaseClassEq

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by keenon on 5/29/15.
 *
 * Allows the use of external models, with our own KNN tuning thrown on top, to power decision making. This should enable
 * multiclass decisionmaking where it was previously too poorly tuned to be possible
 */
abstract class ExternalModelStream[Input](humanErrorDistribution : HumanErrorDistribution) extends ModelStream(humanErrorDistribution) {
  /**
   * This does prediction to get a prior on a single input instance, using the model
   * @param externalModel the input object about which we're making predictions
   * @return a raw distribution over possible tags - don't worry about tuning, we'll handle that
   */
  def rawPrior(externalModel : ExternalModel[Input]) : Map[ExternalModelVariable,Map[String,Double]]

  /**
   * Trains the external model using a set of examples. This can be a no-op if the model is already well trained on some
   * dataset, and we don't want online training
   * @param inputOutputPairs the pairs of (input, output) to use in training, if desired
   */
  def trainExternalModel(inputOutputPairs : Iterable[(Input,Map[ModelVariable,String])]) : Double

  def devSet : List[(ExternalModel[Input], Map[ExternalModelVariable, String])] = List()

  /**
   * This takes care of the KNN tuning on the external model.
   *
   * @param externalModel the model about which we're making predictions
   * @return
   */
  def tunedPrior(externalModel : ExternalModel[Input]) : Map[ExternalModelVariable,Map[String,Double]] = {
    rawPrior(externalModel).map(raw => {
      val rawDistribution = raw._2

      val useKNNSmoothing = true
      if (useKNNSmoothing) {
        // Find K nearest neighbors
        val K = 15
        val M = 1.0

        def sqrDist(map1 : Map[String,Double], map2 : Map[String,Double]) : Double = {
          var dist = 0.0
          for (p <- map1) {
            val v = map2(p._1)
            dist += Math.pow(v - p._2, 2.0)
          }
          dist
        }

        val kNearest = ListBuffer[(Map[String,Double],String)]()
        var maxDistance = Double.PositiveInfinity
        for (pair <- knnList) {
          val dist = sqrDist(pair._1, rawDistribution)

          if (kNearest.size < K) {
            kNearest.append(pair)
          }
          else if (dist < maxDistance) {
            val worstCandidate = kNearest.maxBy(p => sqrDist(p._1, rawDistribution))
            kNearest.-=(worstCandidate)
            kNearest.append(pair)
            maxDistance = kNearest.map(p => sqrDist(p._1, rawDistribution)).max
          }
        }

        val map = mutable.Map[String,Double]()
        for (cl <- raw._1.possibleValues) map.put(cl, M)
        for (pair <- kNearest) map.put(pair._2, map.getOrElse(pair._2, 0.0) + 1.0)

        val sum = map.map(_._2).sum
        val tunedDistribution = map.map(pair => (pair._1, pair._2 / sum)).toMap

        (raw._1, tunedDistribution)
      }
      else {
        (raw._1, rawDistribution)
      }
    })
  }

  var knnList : List[(Map[String,Double],String)] = List()

  /**
   * Retrains the model based on all examples seen so far.
   * @return model loss, for tracking and debugging optimizers
   */
  override def learn(models: Iterable[Model]): Double = {
    val loss = trainExternalModel(models.map(m => (m.asInstanceOf[ExternalModel[Input]].getInput, m.map)))

    val dev = devSet
    if (dev.size > 0) {
      knnList = dev.flatMap(m => {
        val prior = rawPrior(m._1)
        prior.map(p => {
          (p._2, m._2(p._1))
        })
      })
    }
    loss
  }

  /**
   * Gets a new model (an empty graph, or a single logit with no features) from this modelStream, which can then be
   * manipulated and specialized according to the subclass type.
   *
   * @return a new model
   */
  override def newModel(): Model = new ExternalModel[Input](this)
}

class ExternalModel[Input](stream : ModelStream) extends Model(stream) {

  private val externalModelStream : ExternalModelStream[Input] = stream.asInstanceOf[ExternalModelStream[Input]]

  // Deal with setting up the user input

  private var input : Option[Input] = None
  def setInput(i : Input) : Unit = {
    input = Some(i)
  }
  def hasInput : Boolean = {
    input match {
      case None => false
      case Some(i) => true
    }
  }
  def getInput : Input = {
    input match {
      case None => throw new IllegalStateException("Can't access input you never set")
      case Some(i) => i
    }
  }

  // Deal with setting up the variables to use

  private var vars : List[ExternalModelVariable] = List()
  def setVariables(v : List[ExternalModelVariable]): Unit = {
    vars = v
  }

  // Store human observations

  private var humanObs : Map[ModelVariable, Map[String,Int]] = Map()
  private def setHumanObservations(variable : ModelVariable, newObs : String, oldHumanObs : Map[ModelVariable, Map[String,Int]]) : Unit = {
    // Add the map of observations for this variable, if it doesn't exist
    val newHumanObs = mutable.Map[ModelVariable, Map[String,Int]]()
    newHumanObs ++= oldHumanObs
    if (!newHumanObs.contains(variable)) newHumanObs.put(variable, Map())
    // Add the observation count for this variable-obs pair
    val count = mutable.Map[String,Int]()
    count ++= newHumanObs(variable)
    count.put(newObs, count.getOrElse(newObs, 0) + 1)
    newHumanObs.put(variable, count.toMap)
    // Set the permanent value
    humanObs = newHumanObs.toMap
  }

  /**
   * Adds a new observation to the given variable, with updates according to the modelStream.humanErrorDistribution,
   * and returns the new model representing this new state of the world.
   *
   * @param variable the variable to which we will add an observation
   * @param observation the value observed
   * @return a new model, representing adding this observation
   */
  override def protectedCloneModelWithHumanObservation(variable: ModelVariable, observation: String): Model = {
    val newModel = new ExternalModel[Input](stream)
    newModel.setInput(getInput)
    newModel.setVariables(vars)
    newModel.setHumanObservations(variable, observation, humanObs)
    newModel
  }

  /**
   * @return a map of the variables -> distributions over tokens
   */
  override def marginals: Map[ModelVariable, Map[String, Double]] = {
    lazy val tunedPrior = externalModelStream.tunedPrior(this).asInstanceOf[Map[ModelVariable, Map[String,Double]]]

    variables.map(variable => {
      if (variable.isObserved) {
        (variable, variable.possibleValues.map(v => v -> {
          if (variable.getObservedValue == v) 1.0
          else 0.0
        }).toMap)
      }
      else {
        // Get all the variables we will need to multiply our raw marginal by human observations
        val tunedMarginal = tunedPrior(variable)
        val humanObservations = humanObs.getOrElse(variable, Map())
        val humanErrorDistribution = stream.getHumanErrorDistribution
        // Multiply by the human error distribution for all observations
        val humanMarginalsUnnormalized = tunedMarginal.map(p => {
          val hypotheticalValue = p._1
          val priorProbability = p._2

          var probability = priorProbability
          humanObservations.foreach(humanObs => {
            val obs = humanObs._1
            val count = humanObs._2
            probability *= Math.pow(humanErrorDistribution.jointProbability(hypotheticalValue, obs), count)
          })

          (hypotheticalValue, probability)
        })
        // Renormalize the results
        val sum = humanMarginalsUnnormalized.map(_._2).sum
        (variable, humanMarginalsUnnormalized.map(p => (p._1, p._2 / sum)))
      }
    }).toMap
  }

  /**
   * @return a map of variables -> MAP assignment
   */
  override def map: Map[ModelVariable, String] = marginals.map(pair => {
    if (pair._1.isObserved) {
      (pair._1, pair._1.getObservedValue)
    }
    else {
      (pair._1, pair._2.maxBy(_._2)._1)
    }
  })

  /**
   * @return a list of the variables that this model covers
   */
  override def variables: List[ModelVariable] = vars
}

case class ExternalModelVariable(m : Model, possibleValues : List[String], payload : Any = null) extends ModelVariable(m) with CaseClassEq
