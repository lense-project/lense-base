package edu.stanford.lense_base

import java.io.{FileWriter, BufferedWriter, File}

import com.github.keenon.minimalml.GNUPlot
import edu.stanford.lense_base.gameplaying._
import edu.stanford.lense_base.graph.{GraphNode, GraphStream, Graph}
import edu.stanford.lense_base.humancompute._
import edu.stanford.lense_base.models._
import edu.stanford.lense_base.mturk.HITCreator
import edu.stanford.lense_base.server._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParMap
import scala.concurrent.{Await, Promise}
import scala.io.Source
import scala.util.{Random, Try}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by keenon on 5/2/15.
 *
 * This holds the abstract guts of a use case for Lense, which should help minimize crufty overhead when generating new
 * use cases. You can of course still use LenseEngine directly, but this might be a time-saver in general
 */
abstract class LenseUseCase[Input <: Any, Output <: Any] {

  lazy val modelStream: ModelStream = getModelStream
  lazy val lenseEngine: LenseEngine = new LenseEngine(modelStream,
    gamePlayer,
    humanErrorDistribution,
    humanDelayDistribution,
    runLearningThread = useLearning)

  lazy val ensureWorkServer = {
    WorkUnitServlet.engine = lenseEngine
    System.err.println("Starting server")
    WorkUnitServlet.server
  }

  def makeHITAndWaitFor(numberOfHumans: Int) : String = {
    System.err.println("Creating "+numberOfHumans+" MTurk HITs...")
    val hitId = HITCreator.createHIT(1.00, numberOfHumans)

    // Disable waiting for additional connections, just seems to piss off workers...
    System.err.println("Waiting for " + 1 + " to connect...")
    WorkUnitServlet.waitForSimultaneousConnections(1)
    hitId
  }

  def initialize(): Unit = {
    // Add the training data as a list of labeled graphs
    if (useLearning) {
      lenseEngine.addTrainingData(
        initialTrainingData.map(pair =>
          toLabeledModel(pair._1, pair._2)
        )
      )
    }
    // Add budget as initial budget
    lenseEngine.budget.addBudget(budget)
  }

  def humanErrorDistribution : HumanErrorDistribution

  def humanDelayDistribution : HumanDelayDistribution

  def useCaseReportSubpath: String = ""

  def getModelStream : ModelStream

  def toLabeledModel(input : Input, output : Output) : Model = {
    val model = toModel(input)
    val labels = toGoldModelLabels(model, output)
    labels.foreach(pair => pair._1.setObservedValue(pair._2))
    model
  }

  /**
   * This specifies the budget that this run will spend, in dollars. You may not use all of it, but the engine will stop
   * asking humans for help, and revert to simple machine learning, after it has exhausted the budget.
   *
   * @return amount in dollars to use as budget
   */
  def budget: Double

  /**
   * This function takes an Input
   * This must return a model created in the local ModelStream
   *
   * @param input the input that the graph will represent
   * @return a graph representing the input, and taking labels from the output if it is passed in
   */
  def toModel(input: Input): Model

  /**
   * Returns the correct labels for all the variables in the model, given a graph and the corresponding gold output. This
   * is used both for generating initial training data and for doing analysis during testing
   *
   * @param model the model we need to attach labels to
   * @param goldOutput the output we expect from the graph labellings
   * @return
   */
  def toGoldModelLabels(model : Model, goldOutput: Output): Map[ModelVariable, String] = {
    model.variables.map(v => {
      (v, getCorrectLabel(v, goldOutput))
    }).toMap
  }

  def getCorrectLabel(variable : ModelVariable, goldOutput: Output): String

  def getQuestion(variable : ModelVariable, hcu: HumanComputeUnit): GraphNodeQuestion

  /**
   * Reads the MAP assignment out of the values object, and returns an Output corresponding to this graph having these
   * values.
   * The keys of the values map will always correspond one-to-one with the nodes of the graph.
   *
   * @param model the graph, with observedValue's on all the nodes
   * @param values a map corresponding the nodes of the graph with their String labels
   * @return an Output version of this graph
   */
  def toOutput(model : Model, values: Map[ModelVariable, String]): Output

  def encodeModelWithValuesAsTSV(model : Model, values: Map[ModelVariable, String]): String

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
  def lossFunction(mostLikelyGuesses: List[(ModelVariable, String, Double)], cost: Double, ms: Long): Double

  /**
   * Some gameplayers care about losses being expressed as reward (negative loss) in the [0,1] range. To accomplish this,
   * we need to know a max loss per node
   * @return
   */
  def maxLossPerNode: Double

  /**
   * An opportunity to provide some seed data for training the model before the online task begins. This data will
   * be used to train the classifier prior to any testing or online activity
   *
   * @return model seed training data
   */
  def initialTrainingData: List[(Input, Output)] = List()

  /**
   * An opportunity to provide some examples for humans to learn with. The first element in the Tuple is the GraphNode
   * to ask the question about, the second is the correct answer (String), and the third is any explanation to the user
   * during the question (HTML)
   *
   * @return
   */
  def humanTrainingExamples: List[TrainingQuestion] = List()

  /**
   * Get a text introduction to explain the task to Turkers
   * @return
   */
  def humanTrainingIntroduction: String = ""

  /**
   * Get a cheat sheet to display continuously throughout the task
   * @return
   */
  def humanCheatSheet: String = ""

  RealHumanHCUPool.trainingQuestions = {
    if (humanTrainingIntroduction.length > 0) {
      List(IntroductionTrainingQuestion(humanTrainingIntroduction))
    }
    else List()
  } ++ {
    if (humanCheatSheet.length > 0) {
      List(TrainingCheatSheet(humanCheatSheet))
    }
    else List()
  } ++ humanTrainingExamples

  /**
   * An opportunity to provide a new game player, besides the default
   *
   * @return a game player
   */
  def gamePlayer : GamePlayer = new SamplingLookaheadOneHeuristic(humanErrorDistribution, humanDelayDistribution) // ThresholdHeuristic

  /**
   * This is used when calculating recall and F1, if non-null. Useful for cases like NER where there's a dominant NULL class.
   * @return
   */
  def defaultClass : String = null

  /**
   * A hook to be able to render intermediate progress during testWith[...] calls. Intended to print to stdout.
   */
  def renderClassification(model : Model, goldMap : Map[ModelVariable, String], guessMap : Map[ModelVariable, String]) : Unit = {}

  var useLearning : Boolean = true

  ////////////////////////////////////////////////
  //
  //  These are functions that LenseUseCase provides, assuming the above are correct
  //
  ////////////////////////////////////////////////

  def progressivelyAnalyze(goldPairs : List[(Input, Output)],
                           devPairs : List[(Input, Output)],
                           fn : ((Input, Output)) => (Model, Map[ModelVariable,String], Map[ModelVariable,String], PredictionSummary),
                           hcuPool : HCUPool,
                           saveTitle : String) = {
    val mutableAnalysis = mutable.ListBuffer[(Model, Map[ModelVariable,String], Map[ModelVariable,String], PredictionSummary)]()

    val mutableResultSet = mutable.ListBuffer[ResultSet]()

    // Warm up indexes on the dev pairs out of parallel
    for (pair <- devPairs) {
      val model = toModel(pair._1)
      if (model.isInstanceOf[GraphicalModel]) {
        modelStream.asInstanceOf[GraphicalModelStream].graphStream.model.warmUpIndexes(model.asInstanceOf[GraphicalModel].getGraph)
      }
    }

    var i = 0
    for (pair <- goldPairs) {
      mutableAnalysis.+=(fn(pair))
      i += 1
      if (i % 1 == 0) {
        analyzeOutput(mutableAnalysis.toList, hcuPool, devPairs, mutableResultSet, saveTitle)
      }
      if (i % 50 == 0) {
        analyzeConfidence(goldPairs, saveTitle, i)
      }
    }

    // Send home all of our real human labelers when we're done with all of our predictions
    // for a non-infinite stream.

    if (hcuPool != null && hcuPool.hcuPool != null) {
      for (hcu <- hcuPool.hcuPool) {
        hcu match {
          case client: HCUClient => client.completeAndPay()
          case _ =>
        }
      }
    }

    analyzeOutput(mutableAnalysis.toList, hcuPool, devPairs, mutableResultSet, saveTitle)
    analyzeConfidence(goldPairs, saveTitle, i)
  }

  case class ResultSet(fullPrecision : Double, fullRecall : Double, fullF1 : Double, machinePrecision : Double, machineRecall : Double, machineF1 : Double)

  /**
   * This will run a test against artificial humans, with a "probability epsilon choose uniformly at random" error
   * function. It will print results to stdout.
   *
   * @param goldPairs pairs of Input and the corresponding correct Output objects
   * @param humanErrorDistribution the error distribution for artificial humans
   */
  def testWithArtificialHumans(goldPairs : List[(Input, Output)],
                               devPairs : List[(Input, Output)],
                               humanErrorDistribution : HumanErrorDistribution,
                               humanDelayDistribution : HumanDelayDistribution,
                               workUnitCost : Double,
                               startNumArtificialHumans : Int,
                               saveTitle : String) : Unit = {
    initialize()

    val rand = new Random()
    val hcuPool = ArtificialHCUPool(startNumArtificialHumans, humanErrorDistribution, humanDelayDistribution, workUnitCost, rand)

    progressivelyAnalyze(goldPairs, devPairs, pair => {
      val model = toModel(pair._1)
      val goldMap = toGoldModelLabels(model, pair._2)
      for (variable <- model.variables) {
        if (!goldMap.contains(variable)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithArtificialHumans(model, pair._2, humanErrorDistribution, humanDelayDistribution, rand, hcuPool).future, 1000 days)
      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)
      renderClassification(model, goldMap, guessMap._2)
      (guessMap._1, goldMap, guessMap._2, guessMap._3)
    }, hcuPool, saveTitle)

    hcuPool.kill()

    System.exit(0)
  }

  def testBaselineForOfflineLabeling(goldPairs : List[(Input, Output)], devPairs : List[(Input, Output)]) = {
    initialize()

    var numSwapsSoFar = 0

    progressivelyAnalyze(goldPairs, devPairs, pair => {
      val model = toModel(pair._1)
      val goldMap = toGoldModelLabels(model, pair._2)

      val confidenceSet = model.marginals.map(pair => {
        pair._2.maxBy(_._2)._2
      })
      val initialAverageConfidence = confidenceSet.sum / confidenceSet.size
      val initialMinConfidence = confidenceSet.min
      val initialMaxConfidence = confidenceSet.max

      val guessMap = model.map

      // Retrain after each example
      goldMap.foreach(pair => pair._1.setObservedValue(pair._2))
      lenseEngine.pastGuesses += model

      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)

      val idx = goldPairs.indexOf(pair)
      if ((idx < 200 && idx % 40 == 0)  || (idx % 80 == 0)) {
        modelStream.learn(lenseEngine.pastGuesses)
        numSwapsSoFar += 1
      }

      renderClassification(model, goldMap, guessMap)
      val loss = 0
      (model, goldMap, guessMap, PredictionSummary(loss, 0, 0, 0, 0, 0, initialMinConfidence, initialMaxConfidence, initialAverageConfidence, numSwapsSoFar, lenseEngine.currentLoss(), lenseEngine.pastGuesses.size, List(), List(), List(), "turn-in-guess"))
    }, null, "offline_baseline")

    System.exit(0)
  }

  def testBaselineForAllHuman(goldPairs : List[(Input, Output)],
                              devPairs : List[(Input, Output)],
                              humanErrorDistribution : HumanErrorDistribution,
                              humanDelayDistribution : HumanDelayDistribution,
                              workUnitCost : Double,
                              poolSize : Int,
                              numQueriesPerNode : Int,
                              useRealHumans : Boolean = false) : Unit = {
    useLearning = false
    initialize()

    val rand = new Random()
    val hitId = if (useRealHumans) {
      ensureWorkServer
      makeHITAndWaitFor(poolSize)
    } else ""

    val hcuPool : HCUPool = if (useRealHumans) {
      RealHumanHCUPool
    }
    else {
      ArtificialHCUPool(poolSize, humanErrorDistribution, humanDelayDistribution, workUnitCost, rand)
    }

    lenseEngine.gamePlayer = new NQuestionBaseline(numQueriesPerNode)
    lenseEngine.gamePlayer.budget = lenseEngine.budget
    lenseEngine.turnOffLearning()

    progressivelyAnalyze(goldPairs, devPairs, pair => {
      val model = toModel(pair._1)
      val goldMap = toGoldModelLabels(model, pair._2)
      for (variable <- model.variables) {
        if (!goldMap.contains(variable)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = if (useRealHumans) {
        Await.result(classifyWithRealHumans(model, RealHumanHCUPool).future, 1000 days)
      } else {
        Await.result(classifyWithArtificialHumans(model, pair._2, humanErrorDistribution, humanDelayDistribution, rand, hcuPool).future, 1000 days)
      }
      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)
      renderClassification(model, goldMap, guessMap._2)
      (guessMap._1, goldMap, guessMap._2, guessMap._3)
    }, hcuPool, "all_human_"+numQueriesPerNode)

    // We're now done with the run, so we need to expire the HIT, if we haven't already
    if (useRealHumans) {
      HITCreator.expireHIT(hitId)
    }
    else {
      hcuPool.asInstanceOf[ArtificialHCUPool].kill()
    }
  }

  /**
   * This will run a test against real live humans. The LenseUseCase needs to be triggered from inside Jetty, once it's
   * running, so that there's a server to ask for human opinions.
   *
   * @param goldPairs pairs of Input and the corresponding correct Output objects
   */
  def testWithRealHumans(goldPairs : List[(Input, Output)], devPairs : List[(Input, Output)], poolSize : Int) : Unit = {
    initialize()

    ensureWorkServer
    val hitId = makeHITAndWaitFor(poolSize)

    progressivelyAnalyze(goldPairs, devPairs, pair => {
      val model = toModel(pair._1)
      val goldMap = toGoldModelLabels(model, pair._2)
      for (variable <- model.variables) {
        if (!goldMap.contains(variable)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithRealHumans(model, RealHumanHCUPool).future, 1000 days)
      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)
      renderClassification(model, goldMap, guessMap._2)
      (guessMap._1, goldMap, guessMap._2, guessMap._3)
    }, RealHumanHCUPool, "real_human")

    // We're now done with the run, so we need to expire the HIT, if we haven't already
    HITCreator.expireHIT(hitId)
  }

  /**
   * Transforms an input to the desired output, using real humans to collect extra information.
   *
   * @param input the input to be transformed
   * @return the desired output
   */
  def getOutput(input : Input) : Promise[Output] = {
    val graphAndQuestion = toModel(input)
    val promise = Promise[Output]()
    classifyWithRealHumans(graphAndQuestion, RealHumanHCUPool).future.onComplete(t => {
      if (t.isSuccess) {
        promise.complete(Try { toOutput(graphAndQuestion, t.get._2) })
      }
      else {
        promise.complete(Try {
          throw t.failed.get
        })
      }
    })
    promise
  }

  private def analyzeClassifierPerformance(goldPairs : List[(Input, Output)]) : (Double,Double,Double) = {
    val nonDefaultCorrect = ParMap[String, Double]()
    val foundNonDefault = ParMap[String, Double]()
    val guessedNonDefault = ParMap[String, Double]()

    val numThreads = Runtime.getRuntime.availableProcessors()

    val threads = (0 to numThreads-1).map(threadIdx => {
      new Thread(new Runnable {
        override def run(): Unit = {
          var j = threadIdx
          while (j < goldPairs.size) {
            val pair = goldPairs(j)
            val model = toModel(pair._1)
            val map = model.map
            val trueLabels = toGoldModelLabels(model, pair._2)

            for (variable <- model.variables) {
              val trueValue = trueLabels(variable)
              val guessedValue = map(variable)

              if (defaultClass != null) {
                if (trueValue != defaultClass) {
                  foundNonDefault.put(trueValue, foundNonDefault.getOrElse(trueValue, 0.0) + 1)
                }
                if (guessedValue != defaultClass) {
                  guessedNonDefault.put(guessedValue, guessedNonDefault.getOrElse(guessedValue, 0.0) + 1)
                }
                if (trueValue == guessedValue && trueValue != defaultClass) {
                  nonDefaultCorrect.put(trueValue, nonDefaultCorrect.getOrElse(trueValue, 0.0) + 1)
                }
              }
            }
            j += numThreads
          }
        }
      })
    })

    threads.foreach(_.start())
    threads.foreach(_.join())

    if (defaultClass != null) {
      val nonDefaultCorrectSum = nonDefaultCorrect.map(_._2).sum
      val foundNonDefaultSum = foundNonDefault.map(_._2).sum
      val guessedNonDefaultSum = guessedNonDefault.map(_._2).sum

      val precision = nonDefaultCorrectSum / guessedNonDefaultSum
      val recall = nonDefaultCorrectSum / foundNonDefaultSum
      val f1 = 2 * precision * recall / (precision + recall)
      (precision, recall, f1)
    }
    else {
      (0,0,0)
    }
  }

  private def analyzeConfidence(goldPairs : List[(Input, Output)], outputPath : String = "default", iteration : Int) : Unit = {
    val resultsPrefix = "results/"+useCaseReportSubpath+(if (useCaseReportSubpath.endsWith("/")) "" else "/")
    val f = new File(resultsPrefix+outputPath)
    if (!f.exists()) f.mkdirs()

    val numBuckets = 7

    val buckets = mutable.Map[Int, (Int,Int)]()
    for (i <- 0 to numBuckets) {
      buckets.put(i, (0,0))
    }

    for (i <- 0 to iteration-1) {
      val pair = goldPairs(i)

      val model = toModel(pair._1)
      val marginals = model.marginals
      val trueLabels = toGoldModelLabels(model, pair._2)

      for (variable <- model.variables) {
        val variableMarginal = marginals(variable)

        val label = trueLabels(variable)
        for (varProbPair <- variableMarginal) {
          val isCorrect = label == varProbPair._1

          val bucketNum = Math.floor(varProbPair._2 * numBuckets).asInstanceOf[Int]
          if (isCorrect) {
            buckets.put(bucketNum, (buckets(bucketNum)._1 + iteration, buckets(bucketNum)._2))
          }
          else {
            buckets.put(bucketNum, (buckets(bucketNum)._1, buckets(bucketNum)._2 + iteration))
          }
        }
      }
    }

    val plot = new GNUPlot
    val predictedPercentage = (0 to numBuckets-1).map(i => {
      i.asInstanceOf[Double] / numBuckets + (0.5 / numBuckets)
    }).toArray

    val truePercentage = (0 to numBuckets-1).map(i => {
      if (buckets(i)._1 + buckets(i)._2 == 0) {
        var bucketVal = 0.0
        // Look backwards for something we can use, if our buckets aren't granular enough
        for (j <- i-1 to 0) {
          if (j >= 0) {
            if (buckets(j)._1 + buckets(j)._2 > 0 && bucketVal == 0.0) {
              bucketVal = buckets(j)._1.asInstanceOf[Double] / (buckets(j)._1 + buckets(j)._2)
            }
          }
        }
        bucketVal
      }
      else buckets(i)._1.asInstanceOf[Double] / (buckets(i)._1 + buckets(i)._2)
    }).toArray

    plot.addLine(predictedPercentage, truePercentage)
    plot.title = "iteration "+iteration+" tuning"
    plot.yLabel = "true percentage"
    plot.xLabel = "predicted percentage"
    plot.saveAnalysis(resultsPrefix+outputPath+"/tuning")
  }

  private def dumpRawNumbers(path : String, values : List[Double]) : Unit = {
    val cw = new BufferedWriter(new FileWriter(path))
    for (v <- values) {
      cw.write(v+"\n")
    }
    cw.close()
  }

  private def frequencyLinePlot(path : String, quantity : String, values : List[Double]) : Unit = {
    if (values.length == 0) return

    val buckets = Math.max(5, values.size / 50)
    val plot = new GNUPlot

    val max = values.max
    val min = values.min

    val indexes = (0 to buckets).map(i => {
      (i.asInstanceOf[Double] * (max - min) / buckets) + min
    }).toArray
    val bucketArray = mutable.Map[Int,Int]()

    for (v <- values) {
      val normalized = (v - min) / (max - min)
      val b = Math.round(normalized*buckets).asInstanceOf[Int]
      bucketArray.put(b, bucketArray.getOrElse(b, 0) + 1)
    }

    plot.addLine(indexes, (0 to buckets).map(i => bucketArray.getOrElse(i, 0).asInstanceOf[Double]).toArray)

    plot.title = quantity+" frequency"
    plot.yLabel = "frequency"
    plot.xLabel = quantity
    plot.saveAnalysis(path)
  }

  private def printAccuracy(path : String, guesses : List[(String,String)]) : Unit = {
    val correct = guesses.count(pair => pair._1 == pair._2).asInstanceOf[Double]
    val percentage = correct / guesses.size
    val cw = new BufferedWriter(new FileWriter(path))
    cw.write("Accuracy: "+percentage)
    cw.close()
  }

  private def printExamples(path : String, guessesWithContext : List[(String,String,ModelVariable)]) : Unit = {
    val cw = new BufferedWriter(new FileWriter(path))
    for (triple <- guessesWithContext) {
      cw.write(triple._3.model.toString()+"\n")
      cw.write("\t"+triple._3.toString()+"\n")
      cw.write("\tGOLD:"+triple._1+"\n")
      cw.write("\tGUESS:"+triple._2+"\n")
    }
    cw.close()
  }

  private def frobeniusNorm(map1 : Map[List[String],Double], map2 : Map[List[String],Double]) : Double = {
    val map1Sum = map1.map(_._2).sum
    val map1Normalized = map1.map(pair => (pair._1, pair._2 / map1Sum))
    val map2Sum = map2.map(_._2).sum
    val map2Normalized = map2.map(pair => (pair._1, pair._2 / map2Sum))

    var squareSum = 0.0

    for (pair <- map1Normalized) {
      val key = pair._1
      val v1 = pair._2
      val v2 = map2Normalized(key)
      squareSum += Math.pow(v1 - v2, 2.0)
    }

    Math.sqrt(squareSum)
  }

  private def printConditionalNormalizedCSV(path : String, probs : Map[List[String],Double]) : Unit = {
    val cw = new BufferedWriter(new FileWriter(path))
    val keys = probs.map(_._1.head).toList.distinct
    cw.write("COL=GUESS;ROW=GOLD")
    for (key <- keys) {
      cw.write(","+key)
    }

    for (key <- keys) {
      cw.write("\n")
      cw.write(key)

      val subMap = probs.filter(_._1.head == key).map(pair => (pair._1.last, pair._2))
      val sum = subMap.map(_._2).sum
      val normalizedSubMap = subMap.map(pair => (pair._1, pair._2 / sum))

      for (guess <- keys) {
        cw.write(","+normalizedSubMap(guess))
      }
    }

    cw.close()
  }

  private def printHumanErrorBeliefsSummary(path : String, l : List[(Model, Map[ModelVariable, String], Map[ModelVariable, String], PredictionSummary)]) : Unit = {
    val summaries = l.map(_._4)

    val f = new File(path)
    if (!f.exists()) {
      f.mkdirs()
    }

    // Print out underlying distribution in the same format as our guesses

    if (summaries.size > 0) {
      val summary = summaries.head
      var j = 0
      for (pair <- summary.believedVsReceivedHumanDistribution) {
        printConditionalNormalizedCSV(path + "receivedError_type_" + j + ".csv", pair._2)
        j += 1
      }
    }

    val dw = new BufferedWriter(new FileWriter(path+"human_observations.txt"))

    // Log the actual resulting distributions

    val conditionalMatricesPath = path+"conditional_matrices_over_time/"
    val f2 = new File(conditionalMatricesPath)
    if (!f2.exists()) f2.mkdirs()

    var humanRequests = 0
    for (i <- 0 to summaries.length-1) {
      val summary = summaries(i)
      humanRequests += summary.numRequestsCompleted

      if (i % 1 == 0) {
        var j = 0
        for (pair <- summary.believedVsReceivedHumanDistribution) {
          val believedProbabilities = pair._1
          // val receivedProbabilities = pair._2
          dw.write(i.toString+": ")
          dw.write(humanRequests.toString)
          dw.write("\n")

          val believedPath = conditionalMatricesPath+"believedError_iteration_"+i+"_type_"+j+".csv"
          printConditionalNormalizedCSV(believedPath, believedProbabilities)

          j += 1
        }
      }
    }

    val confusionMatricesPath = path+"confusion_matrices_over_time/"
    val f3 = new File(confusionMatricesPath)
    if (!f3.exists()) f3.mkdirs()

    for (i <- 0 to l.length-1) {
      val humanPredictionsVsCorrect : List[(String,String,ModelVariable,HumanComputeUnit)] = l.slice(0,i-1).flatMap(quad => {
        val graph = quad._1
        val goldMap = quad._2
        quad._4.humanQueryResponses.map(triple => {
          (goldMap(triple._1), triple._3, triple._1, triple._2)
        })
      })

      val variableTypes = l.flatMap(_._1.variables).map(_.possibleValues.toSet).distinct.toList
      for (valueSet <- variableTypes) {
        val pairs = humanPredictionsVsCorrect.filter(_._3.possibleValues.toSet == valueSet).map(quad => (quad._1, quad._2))
        printConfusion(confusionMatricesPath+"/confusion_iteration_"+i+"_nodetype_"+variableTypes.indexOf(valueSet)+".csv",
          valueSet.toList,
          pairs)
      }
    }

    dw.close()
  }

  private def printConfusion(path : String, values : List[String], guesses : List[(String,String)]) : Unit = {
    // Get the confusion matrix
    val confusion : mutable.Map[(String,String), Int] = mutable.Map()
    for (pair <- guesses) {
      confusion.put(pair, confusion.getOrElse(pair, 0) + 1)
    }
    // Write out to the file
    val confusionResultFile = new File(path)
    if (!confusionResultFile.exists()) confusionResultFile.createNewFile()
    val cw = new BufferedWriter(new FileWriter(confusionResultFile))
    // Write out header row
    cw.write("COL=GUESS;ROW=GOLD")
    for (guess <- values) {
      cw.write(",")
      cw.write(guess)
    }

    for (gold <- values) {
      cw.write("\n")
      cw.write(gold)
      cw.write(",")
      var j = 0
      for (guess <- values) {
        if (j > 0) {
          cw.write(",")
        }
        j += 1
        cw.write(""+confusion.getOrElse((gold, guess), 0))
      }
    }
    cw.close()
  }

  // Prints some outputs to stdout that are the result of analysis
  private def analyzeOutput(l : List[(Model, Map[ModelVariable, String], Map[ModelVariable, String], PredictionSummary)], hcuPool : HCUPool, goldPairs : List[(Input, Output)], mutableResultSet : mutable.ListBuffer[ResultSet], outputPath : String = "default") : Unit = {
    var correct = 0.0
    var incorrect = 0.0

    val nonDefaultCorrect = mutable.Map[String, Double]()
    val foundNonDefault = mutable.Map[String, Double]()
    val guessedNonDefault = mutable.Map[String, Double]()

    var requested = 0.0
    var completed = 0.0
    var time = 0L
    var tokens = 0
    for (quad <- l) {
      try {
        for (variable <- quad._1.variables) {
          val trueValue = quad._2(variable)
          val guessedValue = quad._3(variable)
          if (trueValue == guessedValue) correct += 1
          else incorrect += 1
          tokens += 1

          if (defaultClass != null) {
            if (trueValue != defaultClass) {
              foundNonDefault.put(trueValue, foundNonDefault.getOrElse(trueValue, 0.0) + 1)
            }
            if (guessedValue != defaultClass) {
              guessedNonDefault.put(guessedValue, guessedNonDefault.getOrElse(guessedValue, 0.0) + 1)
            }
            if (trueValue == guessedValue && trueValue != defaultClass) {
              nonDefaultCorrect.put(trueValue, nonDefaultCorrect.getOrElse(trueValue, 0.0) + 1)
            }
          }
        }
        requested += quad._4.numRequests
        completed += quad._4.numRequestsCompleted
        time += quad._4.timeRequired
      }
      catch {
        case e : Throwable => {
          System.err.println("Had issue while doing analysis")
          e.printStackTrace()
        }
      }
    }
    println("Accuracy: "+(correct/(correct+incorrect)))
    println("Requested task completion percentage: "+(completed / requested))
    if (tokens > 0) {
      println("Avg time/token: " + (time / tokens))
      println("Avg requests/token: " + (requested / tokens))
      println("Avg completed requests/token: " + (completed / tokens))
    }

    val resultsPrefix = "results/"+useCaseReportSubpath+(if (useCaseReportSubpath.endsWith("/")) "" else "/")

    val f = new File(resultsPrefix+outputPath)
    if (!f.exists()) f.mkdirs()
    if (!f.isDirectory) {
      f.delete()
      f.mkdir()
    }

    val summaryResultFile = new File(resultsPrefix+outputPath+"/summary.txt")
    if (!summaryResultFile.exists()) summaryResultFile.createNewFile()
    val bw = new BufferedWriter(new FileWriter(summaryResultFile))
    bw.write("Num Examples Processed: "+l.size+"\n")
    bw.write("Individual Nodes: "+(correct + incorrect)+"\n")
    bw.write("Correct: "+correct+"\n")
    bw.write("Incorrect: "+incorrect+"\n")
    bw.write("Accuracy: "+(correct/(correct+incorrect))+"\n")
    bw.write("Requested task completion percentage: "+(completed / requested)+"\n")
    if (tokens > 0) {
      bw.write("Avg time/token: " + (time / tokens) + "\n")
      bw.write("Avg requests/token: " + (requested / tokens) + "\n")
      bw.write("Avg completed requests/token: " + (completed / tokens) + "\n")
    }

    var mainPrecision = 0.0
    var mainRecall = 0.0
    var mainF1 = 0.0

    if (defaultClass != null) {
      val nonDefaultCorrectSum = nonDefaultCorrect.map(_._2).sum
      val foundNonDefaultSum = foundNonDefault.map(_._2).sum
      val guessedNonDefaultSum = guessedNonDefault.map(_._2).sum

      val precision = nonDefaultCorrectSum / guessedNonDefaultSum
      val recall = nonDefaultCorrectSum / foundNonDefaultSum
      val f1 = 2*precision*recall / (precision + recall)

      mainPrecision = precision
      mainRecall = recall
      mainF1 = f1

      bw.write("\nOverall performance:\n")

      bw.write("\tNon-Default Gold Token Count:"+foundNonDefaultSum+"\n")
      bw.write("\tNon-Default Guessed Token Count:"+guessedNonDefaultSum+"\n")

      bw.write("\tPrecision: "+precision+"\n")
      bw.write("\tRecall: "+recall+"\n")
      bw.write("\tF1: "+f1+"\n")

      // For every observed class, do the same thing locally
      for (cl <- foundNonDefault.map(_._1)) {
        bw.write("\n"+cl+":\n")

        val localPrecision = nonDefaultCorrect.getOrElse(cl, 0.0) / guessedNonDefault.getOrElse(cl, 0.0)
        val localRecall = nonDefaultCorrect.getOrElse(cl, 0.0) / foundNonDefault.getOrElse(cl, 0.0)
        val localF1 = 2*localPrecision*localRecall / (localPrecision + localRecall)
        bw.write("\tNon-Default Gold Token Count:"+foundNonDefault.getOrElse(cl, 0)+"\n")
        bw.write("\tNon-Default Guessed Token Count:"+guessedNonDefault.getOrElse(cl, 0)+"\n")
        bw.write("\tPrecision: "+localPrecision+"\n")
        bw.write("\tRecall: "+localRecall+"\n")
        bw.write("\tF1: "+localF1+"\n")
      }
    }
    bw.close()

    // Record performance over time

    val machinePerformance = analyzeClassifierPerformance(goldPairs)
    val machinePrecision = machinePerformance._1
    val machineRecall = machinePerformance._2
    val machineF1 = machinePerformance._3

    def nanSafe(d : Double) = if (d.isNaN) 0.0 else d
    mutableResultSet += ResultSet(nanSafe(mainPrecision), nanSafe(mainRecall), nanSafe(mainF1), nanSafe(machinePrecision), nanSafe(machineRecall), nanSafe(machineF1))

    // Print the actual output guess and gold

    val gw = new BufferedWriter(new FileWriter(resultsPrefix+outputPath+"/gold.tsv"))
    val aw = new BufferedWriter(new FileWriter(resultsPrefix+outputPath+"/guess.tsv"))
    for (quad <- l) {
      val goldEncoded = encodeModelWithValuesAsTSV(quad._1, quad._2)
      gw.write(goldEncoded)
      if (!goldEncoded.endsWith("\n")) gw.write("\n")
      gw.write("\n")

      val guessEncoded = encodeModelWithValuesAsTSV(quad._1, quad._3)
      aw.write(guessEncoded)
      if (!guessEncoded.endsWith("\n")) aw.write("\n")
      aw.write("\n")
    }
    gw.close()
    aw.close()

    // Print the logs of behavior, separated by perfect runs and mistakes for convenience

    val perfectRuns = l.filter(quad => quad._2 == quad._3)
    val imperfectRuns = l.filter(!perfectRuns.contains(_))

    val pw = new BufferedWriter(new FileWriter(resultsPrefix+outputPath+"/perfect-run-logs.txt"))
    for (quad <- perfectRuns) {
      pw.write(quad._4.behaviorLog)
      pw.write("\n\n\n")
    }
    pw.close()

    val iw = new BufferedWriter(new FileWriter(resultsPrefix+outputPath+"/imperfect-run-logs.txt"))
    for (quad <- imperfectRuns) {
      iw.write(quad._4.behaviorLog)
      iw.write("\n\n\n")
    }
    iw.close()

    // Print the confusion matrix for the output classes

    val variableTypes = l.flatMap(_._1.variables).map(_.possibleValues.toSet).distinct.toList
    for (valueSet <- variableTypes) {
      printConfusion(resultsPrefix+outputPath+"/confusion_nodetype_"+variableTypes.indexOf(valueSet)+".csv",
        valueSet.toList,
        l.flatMap(quad => {
          quad._1.variables.filter(_.possibleValues.toSet == valueSet).map(variable => (quad._2(variable), quad._3(variable)))
        }))
    }

    // Gold, guess, HCU responsible
    val humanPredictionsVsCorrect : List[(String,String,ModelVariable,HumanComputeUnit)] = l.flatMap(quad => {
      val graph = quad._1
      val goldMap = quad._2
      quad._4.humanQueryResponses.map(triple => {
        (goldMap(triple._1), triple._3, triple._1, triple._2)
      })
    })
    val hcusInvolved = humanPredictionsVsCorrect.map(_._4).distinct

    def smoothTimeSeries(timeSeriesData : Array[Double], timeToAvg : Int) : Array[Double] = {
      var sum = 0.0
      val newData = new Array[Double](timeSeriesData.length)
      for (i <- 0 to timeSeriesData.length-1) {
        sum += timeSeriesData(i)
        if (i < timeToAvg) {
          newData.update(i, sum / (i+1))
        }
        else {
          sum -= timeSeriesData(i-timeToAvg)
          newData.update(i, sum / timeToAvg)
        }
      }
      newData
    }

    val queries = (0 to l.size-1).toArray.map(i => i.asInstanceOf[Double])
    val hotSwapIndexes : List[Int] = l.zipWithIndex.flatMap(pair => {
      val idx = pair._2
      if (idx > 0) {
        val thisPrediction = pair._1._4
        val lastPrediction = l(idx-1)._4
        if (thisPrediction.numSwapsSoFar > lastPrediction.numSwapsSoFar) {
          List(idx)
        }
        else {
          List[Int]()
        }
      }
      else {
        List[Int]()
      }
    })

    def plotAgainstQueries(yLabel : String, timeSeriesData : Array[Double]) = {
      plotAgainst(resultsPrefix+outputPath+"/", "time", queries, yLabel, timeSeriesData)
    }

    def plotAgainst(path : String, xLabel : String, xData : Array[Double], yLabel : String, yData : Array[Double]) = {
      val plot = new GNUPlot
      plot.addLine(xData, yData)
      plot.addLine(xData, smoothTimeSeries(yData, 30))
      plot.addLine(xData, smoothTimeSeries(yData, 100))
      plot.title = yLabel+" vs "+xLabel
      plot.yLabel = yLabel
      plot.xLabel = xLabel
      for (idx <- hotSwapIndexes) {
        plot.addVerticalLine(idx)
      }
      plot.saveAnalysis(path+yLabel.replaceAll(" ","_")+"_plot")
    }

    // Do overall HCU collective analysis
    val hcuPrefix = resultsPrefix+outputPath+"/hcu_report/"
    val hcuReportFolder = new File(hcuPrefix)
    if (!hcuReportFolder.exists()) hcuReportFolder.mkdirs()
    frequencyLinePlot(hcuPrefix+"overall_latency_curve", "latency", l.flatMap(_._4.humanQueryDelays.map(_._2.asInstanceOf[Double])).toList)
    dumpRawNumbers(hcuPrefix+"overall_latency_data.txt", l.flatMap(_._4.humanQueryDelays.map(_._2.asInstanceOf[Double])).toList)

    // Plot human confusion matrices

    val summaries = l.map(_._4)
    val errorBeliefsPrefix = hcuPrefix+"error_beliefs/"
    printHumanErrorBeliefsSummary(errorBeliefsPrefix, l)

    // Plot performance over time, according to saved logs

    plotAgainstQueries("f1", mutableResultSet.map(_.fullF1).toArray)
    plotAgainstQueries("recall", mutableResultSet.map(_.fullRecall).toArray)
    plotAgainstQueries("precision", mutableResultSet.map(_.fullPrecision).toArray)
    plotAgainstQueries("machine_f1", mutableResultSet.map(_.machineF1).toArray)
    plotAgainstQueries("machine_recall", mutableResultSet.map(_.machineRecall).toArray)
    plotAgainstQueries("machine_precision", mutableResultSet.map(_.machinePrecision).toArray)

    plotAgainstQueries("model training loss", l.map(quad => quad._4.modelTrainingLoss).toArray)
    plotAgainstQueries("model training loss per example", l.map(quad => quad._4.modelTrainingLoss / quad._4.numExamplesSeen).toArray)

    // Plot the error in our assessment over number of queries observed

    val humanObsOverTime : List[Int] = summaries.scanLeft(0)((sum, summary) => {
      sum + summary.numRequestsCompleted
    }).tail
    val frobeniusOverTime : List[Double] = summaries.map(summary => {
      summary.believedVsReceivedHumanDistribution.map(pair => frobeniusNorm(pair._1, pair._2)).sum / summary.believedVsReceivedHumanDistribution.length
    })
    plotAgainst(errorBeliefsPrefix, "queries", humanObsOverTime.map(_.asInstanceOf[Double]).toArray, "error in beliefs", frobeniusOverTime.toArray)

    for (nodeType <- variableTypes) {
      val pairs = humanPredictionsVsCorrect.filter(_._3.possibleValues.toSet == nodeType).map(quad => (quad._1, quad._2))
      printConfusion(hcuPrefix+"overall_confusion_nodetype_"+variableTypes.indexOf(nodeType)+".csv",
        nodeType.toList,
        pairs)
      printAccuracy(hcuPrefix+"overall_accuracy_nodetype_"+variableTypes.indexOf(nodeType)+".txt", pairs)
      printExamples(hcuPrefix+"overall_examples_nodetype_"+variableTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.map(quad => (quad._1, quad._2, quad._3)))
      printExamples(hcuPrefix+"overall_correct_examples_nodetype_"+variableTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(q => q._1 == q._2).map(quad => (quad._1, quad._2, quad._3)))
      printExamples(hcuPrefix+"overall_incorrect_examples_nodetype_"+variableTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(q => q._1 != q._2).map(quad => (quad._1, quad._2, quad._3)))
    }
    // Do individual HCU level analysis
    for (hcu <- hcusInvolved) {
      val thisHcuPrefix = hcuPrefix+"/"+hcu.getName+"/"
      val thisHcuReportFolder = new File(thisHcuPrefix)
      if (!thisHcuReportFolder.exists()) thisHcuReportFolder.mkdirs()

      val numClassifications = humanPredictionsVsCorrect.count(_._4 eq hcu)
      val cost = numClassifications*hcu.cost
      val ew = new BufferedWriter(new FileWriter(thisHcuPrefix+"cost.txt"))
      ew.write("Classification Decisions: "+numClassifications+"\n")
      ew.write("Amount owed: $"+cost)
      ew.close()

      for (nodeType <- variableTypes) {
        val pairs = humanPredictionsVsCorrect.filter(_._3.possibleValues.toSet == nodeType).filter(_._4 eq hcu).map(quad => (quad._1, quad._2))
        printConfusion(thisHcuPrefix+"confusion_nodetype_"+variableTypes.indexOf(nodeType)+".csv",
          nodeType.toList,
          pairs)
        printAccuracy(thisHcuPrefix+"accuracy_nodetype_"+variableTypes.indexOf(nodeType)+".txt", pairs)
        printExamples(thisHcuPrefix+"examples_nodetype_"+variableTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(_._4 eq hcu).map(quad => (quad._1, quad._2, quad._3)))
        printExamples(thisHcuPrefix+"correct_examples_nodetype_"+variableTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(_._4 eq hcu).filter(q => q._1 == q._2).map(quad => (quad._1, quad._2, quad._3)))
        printExamples(thisHcuPrefix+"incorrect_examples_nodetype_"+variableTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(_._4 eq hcu).filter(q => q._1 != q._2).map(quad => (quad._1, quad._2, quad._3)))
      }

      frequencyLinePlot(thisHcuPrefix+"latency_curve", "latency", l.flatMap(_._4.humanQueryDelays.filter(_._1 eq hcu).map(_._2.asInstanceOf[Double])).toList)
      dumpRawNumbers(thisHcuPrefix+"latency_data.txt", l.flatMap(_._4.humanQueryDelays.filter(_._1 eq hcu).map(_._2.asInstanceOf[Double])).toList)
    }
    plotAgainstQueries("loss per token", l.map(quad => quad._4.loss / quad._1.variables.size).toArray)
    plotAgainstQueries("cost per token", l.map(quad => quad._4.requestCost / quad._1.variables.size).toArray)
    plotAgainstQueries("queries per token", l.map(quad => quad._4.numRequests.asInstanceOf[Double] / quad._1.variables.size).toArray)
    plotAgainstQueries("delay per token", l.map(quad => quad._4.timeRequired.asInstanceOf[Double] / quad._1.variables.size).toArray)
    plotAgainstQueries("prior min confidence", l.map(quad => quad._4.initialMinConfidence).toArray)
    plotAgainstQueries("prior max confidence", l.map(quad => quad._4.initialMaxConfidence).toArray)
    plotAgainstQueries("prior avg confidence", l.map(quad => quad._4.initialAvgConfidence).toArray)
    plotAgainstQueries("accuracy", l.map(quad => {
      var localCorrect = 0.0
      var localIncorrect = 0.0
      for (variable <- quad._1.variables) {
        val trueValue = quad._2(variable)
        val guessedValue = quad._3(variable)
        if (trueValue == guessedValue) localCorrect += 1
        else localIncorrect += 1
      }
      localCorrect / (localCorrect + localIncorrect)
    }).toArray)


    println("Detailed charts printed to \""+resultsPrefix+outputPath+"\"")
  }

  private def classifyWithRealHumans(model : Model, hcuPool : HCUPool) : Promise[(Model, Map[ModelVariable, String],PredictionSummary)] = {
    // Just ensure that we have a server up, this will start it if it isn't running yet
    WorkUnitServlet.server
    // Run actual prediction
    lenseEngine.predict(model, (v, hcu) => getQuestion(v, hcu).getHumanOpinion, hcuPool, lossFunction, maxLossPerNode)
  }

  private def classifyWithArtificialHumans(model : Model,
                                           output : Output,
                                           humanErrorDistribution : HumanErrorDistribution,
                                           humanDelayDistribution : HumanDelayDistribution,
                                           rand : Random,
                                           hcuPool : HCUPool) : Promise[(Model, Map[ModelVariable, String],PredictionSummary)] = {
    lenseEngine.predict(model, (variable, hcu) => {
      val correct = getCorrectLabel(variable, output)

      val promise = Promise[String]()
      val workUnit = ArtificialWorkUnit(promise, humanErrorDistribution.guess(correct, variable.possibleValues), variable, hcuPool)
      hcu.addWorkUnit(workUnit)
      workUnit
    },
    hcuPool,
    lossFunction,
    maxLossPerNode)
  }
}

case class ArtificialHCUPool(startNumHumans : Int, humanErrorDistribution : HumanErrorDistribution, humanDelayDistribution : HumanDelayDistribution, workUnitCost : Double, rand : Random) extends HCUPool {
  var hcuIndex = 0

  for (i <- 1 to startNumHumans) {
    addHCU(new ArtificialComputeUnit(humanErrorDistribution, humanDelayDistribution, workUnitCost, rand, hcuIndex))
    hcuIndex += 1
  }

  var running = true

  def kill(): Unit = {
    running = false
  }

  val churn = false

  // constantly be randomly adding and removing artificial HCU's
  if (churn) {
    new Thread {
      override def run(): Unit = {
        while (true) {
          Thread.sleep(Math.max(5000, 20000 + Math.round(rand.nextGaussian() * 10000).asInstanceOf[Int]))
          if (!running) return

          // at random, remove a human
          if (rand.nextBoolean() && hcuPool.size > 0) {
            System.err.println("** ARTIFICIAL HCU POOL ** Randomly removing an artificial human annotator")
            removeHCU(hcuPool.toList(rand.nextInt(hcuPool.size)))
          }
          // or add a human
          else {
            System.err.println("** ARTIFICIAL HCU POOL ** Randomly adding a human annotator")
            addHCU(new ArtificialComputeUnit(humanErrorDistribution, humanDelayDistribution, workUnitCost, rand, hcuIndex))
            hcuIndex += 1
          }
        }
      }
    }.start()
  }
}

case class ArtificialWorkUnit(resultPromise : Promise[String], guess : String, initVariable : ModelVariable, hcuPool : HCUPool) extends WorkUnit(resultPromise, initVariable, hcuPool)

class ArtificialComputeUnit(humanErrorDistribution : HumanErrorDistribution, humanDelayDistribution : HumanDelayDistribution, workUnitCost : Double, rand : Random, index : Int) extends HumanComputeUnit {
  // Gets the estimated required time to perform this task, in milliseconds
  override def estimateRequiredTimeToFinishItem(variable : ModelVariable): Long = 1000

  // Cancel the current job
  override def cancelCurrentWork(): Unit = {}

  // Get the cost
  override def cost: Double = workUnitCost

  // Kick off a job
  override def startWork(workUnit: WorkUnit): Unit = {
    workUnit match {
      case artificial : ArtificialWorkUnit => {
        // Complete after a gaussian delay
        new Thread {
          override def run() = {
            // Humans can never take less than 1s to make a classification
            val msDelay = humanDelayDistribution.sampleDelay()
            if (msDelay > 0) {
              Thread.sleep(msDelay)
              if (workUnit eq currentWork) {
                finishWork(workUnit, artificial.guess)
              }
            }
          }
        }.start()
      }
    }
  }

  override def getName: String = "artificial_human_"+index
}

case class GraphNodeAnswer(displayText : String, internalClassName : String)
case class GraphNodeQuestion(questionToDisplay : String, possibleAnswers : List[GraphNodeAnswer], hcu : HumanComputeUnit, variable : ModelVariable) {
  def getHumanOpinion : WorkUnit = {
    val p = Promise[String]()
    val workUnit = new MulticlassQuestion(
      questionToDisplay,
      possibleAnswers.map(possibleAnswer => (possibleAnswer.displayText, possibleAnswer.internalClassName)),
      p,
      variable
    )
    hcu.addWorkUnit(workUnit)
    workUnit
  }
}
