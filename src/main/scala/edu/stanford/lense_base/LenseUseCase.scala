package edu.stanford.lense_base

import java.io.{FileWriter, BufferedWriter, File}

import com.github.keenon.minimalml.GNUPlot
import edu.stanford.lense_base.gameplaying._
import edu.stanford.lense_base.graph.{GraphNode, GraphStream, Graph}
import edu.stanford.lense_base.humancompute.{HumanComputeUnit, HCUPool, WorkUnit}
import edu.stanford.lense_base.mturk.HITCreator
import edu.stanford.lense_base.server._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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

  lazy val graphStream: GraphStream = new GraphStream()
  lazy val lenseEngine: LenseEngine = new LenseEngine(graphStream,
    gamePlayer,
    humanErrorDistribution,
    humanDelayDistribution,
    useKNNTuning)

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
    lenseEngine.addTrainingData(
      initialTrainingData.map(pair =>
        toLabeledGraph(pair._1, pair._2)
      )
    )
    // Add budget as initial budget
    lenseEngine.addBudget(budget)
  }

  initialize()

  def humanErrorDistribution : HumanErrorDistribution

  def humanDelayDistribution : HumanDelayDistribution

  def useCaseReportSubpath: String = ""

  /**
   * This specifies the budget that this run will spend, in dollars. You may not use all of it, but the engine will stop
   * asking humans for help, and revert to simple machine learning, after it has exhausted the budget.
   *
   * @return amount in dollars to use as budget
   */
  def budget: Double

  /**
   * This function takes an Input
   * This must return a graph created in the local GraphStream, and it should create a GraphNodeQuestion for each node
   * in the created graph, in case we want to query humans about it.
   *
   * @param input the input that the graph will represent
   * @return a graph representing the input, and taking labels from the output if it is passed in
   */
  def toGraph(input: Input): Graph

  /**
   * Returns the correct labels for all the nodes in the graph, given a graph and the corresponding gold output. This
   * is used both for generating initial training data and for doing analysis during testing
   *
   * @param graph the graph we need to attach labels to
   * @param goldOutput the output we expect from the graph labellings
   * @return
   */
  def toGoldGraphLabels(graph: Graph, goldOutput: Output): Map[GraphNode, String] = {
    graph.nodes.map(n => {
      (n, getCorrectLabel(n, goldOutput))
    }).toMap
  }

  def getCorrectLabel(node: GraphNode, goldOutput: Output): String

  def getQuestion(node: GraphNode, hcu: HumanComputeUnit): GraphNodeQuestion

  /**
   * Reads the MAP assignment out of the values object, and returns an Output corresponding to this graph having these
   * values.
   * The keys of the values map will always correspond one-to-one with the nodes of the graph.
   *
   * @param graph the graph, with observedValue's on all the nodes
   * @param values a map corresponding the nodes of the graph with their String labels
   * @return an Output version of this graph
   */
  def toOutput(graph: Graph, values: Map[GraphNode, String]): Output

  def encodeGraphWithValuesAsTSV(graph: Graph, values: Map[GraphNode, String]): String

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
  def lossFunction(mostLikelyGuesses: List[(GraphNode, String, Double)], cost: Double, ms: Long): Double

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
  def gamePlayer : GamePlayer = ThresholdHeuristic

  /**
   * This is used when calculating recall and F1, if non-null. Useful for cases like NER where there's a dominant NULL class.
   * @return
   */
  def defaultClass : String = null

  /**
   * A hook to be able to render intermediate progress during testWith[...] calls. Intended to print to stdout.
   */
  def renderClassification(graph : Graph, goldMap : Map[GraphNode, String], guessMap : Map[GraphNode, String]) : Unit = {}

  /**
   *
   * @return
   */
  def useKNNTuning : Boolean = false

  ////////////////////////////////////////////////
  //
  //  These are functions that LenseUseCase provides, assuming the above are correct
  //
  ////////////////////////////////////////////////

  def progressivelyAnalyze(goldPairs : List[(Input, Output)],
                           fn : ((Input, Output)) => (Graph, Map[GraphNode,String], Map[GraphNode,String], PredictionSummary),
                           hcuPool : HCUPool,
                           saveTitle : String) = {
    val mutableAnalysis = mutable.ListBuffer[(Graph, Map[GraphNode,String], Map[GraphNode,String], PredictionSummary)]()

    var i = 0
    for (pair <- goldPairs) {
      mutableAnalysis.+=(fn(pair))
      i += 1
      if (i % 2 == 0) {
        analyzeOutput(mutableAnalysis.toList, hcuPool, saveTitle)
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

    analyzeOutput(mutableAnalysis.toList, hcuPool, saveTitle)
    analyzeConfidence(goldPairs, saveTitle, i)
  }

  /**
   * This will run a test against artificial humans, with a "probability epsilon choose uniformly at random" error
   * function. It will print results to stdout.
   *
   * @param goldPairs pairs of Input and the corresponding correct Output objects
   * @param humanErrorDistribution the error distribution for artificial humans
   */
  def testWithArtificialHumans(goldPairs : List[(Input, Output)],
                               humanErrorDistribution : HumanErrorDistribution,
                               humanDelayDistribution : HumanDelayDistribution,
                               workUnitCost : Double,
                               startNumArtificialHumans : Int,
                               saveTitle : String) : Unit = {
    val rand = new Random()
    val hcuPool = ArtificialHCUPool(startNumArtificialHumans, humanErrorDistribution, humanDelayDistribution, workUnitCost, rand)

    progressivelyAnalyze(goldPairs, pair => {
      val graph = toGraph(pair._1)
      val goldMap = toGoldGraphLabels(graph, pair._2)
      for (node <- graph.nodes) {
        if (!goldMap.contains(node)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithArtificialHumans(graph, pair._2, humanErrorDistribution, humanDelayDistribution, rand, hcuPool).future, 1000 days)
      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)
      renderClassification(graph, goldMap, guessMap._1)
      (graph, goldMap, guessMap._1, guessMap._2)
    }, hcuPool, saveTitle)

    hcuPool.kill()

    System.exit(0)
  }

  def testBaselineForOfflineLabeling(goldPairs : List[(Input, Output)]) = {
    var numSwapsSoFar = 0

    progressivelyAnalyze(goldPairs, pair => {
      val graph = toGraph(pair._1)
      val goldMap = toGoldGraphLabels(graph, pair._2)

      val confidenceSet = graph.marginalEstimate().map(pair => {
        pair._2.maxBy(_._2)._2
      })
      val initialAverageConfidence = confidenceSet.sum / confidenceSet.size
      val initialMinConfidence = confidenceSet.min
      val initialMaxConfidence = confidenceSet.max

      val guessMap = graph.mapEstimate()

      // Retrain after each example
      val cloneGraphPair = graph.clone()
      for (n <- graph.nodes) {
        cloneGraphPair._2(n).observedValue = goldMap(n)
      }
      lenseEngine.pastGuesses += cloneGraphPair._1

      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)

      val idx = goldPairs.indexOf(pair)
      if ((idx < 100 && idx % 10 == 0) || (idx < 200 && idx % 20 == 0)  || (idx % 80 == 0)) {
        lenseEngine.learnHoldingPastGuessesConstant()
        numSwapsSoFar += 1
      }

      renderClassification(graph, goldMap, guessMap)
      val loss = 0
      (graph, goldMap, guessMap, PredictionSummary(loss, 0, 0, 0, 0, 0, initialMinConfidence, initialMaxConfidence, initialAverageConfidence, numSwapsSoFar, lenseEngine.currentLoss(), lenseEngine.pastGuesses.size, List(), List(), "turn-in-guess"))
    }, null, "offline_baseline")

    System.exit(0)
  }

  def testBaselineForAllHuman(goldPairs : List[(Input, Output)],
                              humanErrorDistribution : HumanErrorDistribution,
                              humanDelayDistribution : HumanDelayDistribution,
                              workUnitCost : Double,
                              startNumArtificialHumans : Int,
                              numQueriesPerNode : Int) : Unit = {
    val rand = new Random()
    val hcuPool = ArtificialHCUPool(startNumArtificialHumans, humanErrorDistribution, humanDelayDistribution, workUnitCost, rand)

    lenseEngine.gamePlayer = new NQuestionBaseline(numQueriesPerNode)
    lenseEngine.turnOffLearning()

    progressivelyAnalyze(goldPairs, pair => {
      val graph = toGraph(pair._1)
      val goldMap = toGoldGraphLabels(graph, pair._2)
      for (node <- graph.nodes) {
        if (!goldMap.contains(node)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithArtificialHumans(graph, pair._2, humanErrorDistribution, humanDelayDistribution, rand, hcuPool).future, 1000 days)
      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)
      renderClassification(graph, goldMap, guessMap._1)
      (graph, goldMap, guessMap._1, guessMap._2)
    }, hcuPool, "all_human_"+numQueriesPerNode)

    hcuPool.kill()

    System.exit(0)
  }

  /**
   * This will run a test against real live humans. The LenseUseCase needs to be triggered from inside Jetty, once it's
   * running, so that there's a server to ask for human opinions.
   *
   * @param goldPairs pairs of Input and the corresponding correct Output objects
   */
  def testWithRealHumans(goldPairs : List[(Input, Output)], poolSize : Int) : Unit = {
    ensureWorkServer

    val hitId = makeHITAndWaitFor(poolSize)

    progressivelyAnalyze(goldPairs, pair => {
      val graph = toGraph(pair._1)
      val goldMap = toGoldGraphLabels(graph, pair._2)
      for (node <- graph.nodes) {
        if (!goldMap.contains(node)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithRealHumans(graph, RealHumanHCUPool).future, 1000 days)
      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)
      renderClassification(graph, goldMap, guessMap._1)
      (graph, goldMap, guessMap._1, guessMap._2)
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
    val graphAndQuestion = toGraph(input)
    val promise = Promise[Output]()
    classifyWithRealHumans(graphAndQuestion, RealHumanHCUPool).future.onComplete(t => {
      if (t.isSuccess) {
        promise.complete(Try { toOutput(graphAndQuestion, t.get._1) })
      }
      else {
        promise.complete(Try {
          throw t.failed.get
        })
      }
    })
    promise
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

      val graph = toGraph(pair._1)
      val marginals = graph.marginalEstimate()
      val trueLabels = toGoldGraphLabels(graph, pair._2)

      for (node <- graph.nodes) {
        val nodeMarginal = marginals(node)

        val label = trueLabels(node)
        for (nodeProbPair <- nodeMarginal) {
          val isCorrect = label == nodeProbPair._1

          val bucketNum = Math.floor(nodeProbPair._2 * numBuckets).asInstanceOf[Int]
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

  private def printExamples(path : String, guessesWithContext : List[(String,String,GraphNode)]) : Unit = {
    val cw = new BufferedWriter(new FileWriter(path))
    for (triple <- guessesWithContext) {
      cw.write(triple._3.graph.toString()+"\n")
      cw.write("\t"+triple._3.toString()+"\n")
      cw.write("\tGOLD:"+triple._1+"\n")
      cw.write("\tGUESS:"+triple._2+"\n")
    }
    cw.close()
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
    cw.write("\n")

    for (gold <- values) {
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
      cw.write("\n")
    }
    cw.close()
  }

  // Prints some outputs to stdout that are the result of analysis
  private def analyzeOutput(l : List[(Graph, Map[GraphNode, String], Map[GraphNode, String], PredictionSummary)], hcuPool : HCUPool, outputPath : String = "default") : Unit = {
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
        for (node <- quad._1.nodes) {
          val trueValue = quad._2(node)
          val guessedValue = quad._3(node)
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
    println("Avg time/token: "+(time / tokens))
    println("Avg requests/token: "+(requested / tokens))
    println("Avg completed requests/token: "+(completed / tokens))

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
    bw.write("Avg time/token: "+(time / tokens)+"\n")
    bw.write("Avg requests/token: "+(requested / tokens)+"\n")
    bw.write("Avg completed requests/token: "+(completed / tokens)+"\n")
    if (defaultClass != null) {
      val nonDefaultCorrectSum = nonDefaultCorrect.map(_._2).sum
      val foundNonDefaultSum = foundNonDefault.map(_._2).sum
      val guessedNonDefaultSum = guessedNonDefault.map(_._2).sum

      val precision = nonDefaultCorrectSum / guessedNonDefaultSum
      val recall = nonDefaultCorrectSum / foundNonDefaultSum
      val f1 = 2*precision*recall / (precision + recall)

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

    // Print the actual output guess and gold

    val gw = new BufferedWriter(new FileWriter(resultsPrefix+outputPath+"/gold.tsv"))
    val aw = new BufferedWriter(new FileWriter(resultsPrefix+outputPath+"/guess.tsv"))
    for (quad <- l) {
      val goldEncoded = encodeGraphWithValuesAsTSV(quad._1, quad._2)
      gw.write(goldEncoded)
      if (!goldEncoded.endsWith("\n")) gw.write("\n")
      gw.write("\n")

      val guessEncoded = encodeGraphWithValuesAsTSV(quad._1, quad._3)
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

    val nodeTypes = l.flatMap(_._1.nodes).map(_.nodeType).distinct.toList
    for (nodeType <- nodeTypes) {
      printConfusion(resultsPrefix+outputPath+"/confusion_nodetype_"+nodeTypes.indexOf(nodeType)+".csv",
        nodeType.possibleValues.toList,
        l.flatMap(quad => {
          quad._1.nodes.filter(_.nodeType eq nodeType).map(node => (quad._2(node), quad._3(node)))
        }))
    }

    // Gold, guess, HCU responsible
    val humanPredictionsVsCorrect : List[(String,String,GraphNode,HumanComputeUnit)] = l.flatMap(quad => {
      val graph = quad._1
      val goldMap = quad._2
      quad._4.humanQueryResponses.map(triple => {
        (goldMap(triple._1), triple._3, triple._1, triple._2)
      })
    })
    val hcusInvolved = humanPredictionsVsCorrect.map(_._4).distinct

    // Do overall HCU collective analysis
    val hcuPrefix = resultsPrefix+outputPath+"/hcu_report/"
    val hcuReportFolder = new File(hcuPrefix)
    if (!hcuReportFolder.exists()) hcuReportFolder.mkdirs()
    frequencyLinePlot(hcuPrefix+"overall_latency_curve", "latency", l.flatMap(_._4.humanQueryDelays.map(_._2.asInstanceOf[Double])).toList)
    dumpRawNumbers(hcuPrefix+"overall_latency_data.txt", l.flatMap(_._4.humanQueryDelays.map(_._2.asInstanceOf[Double])).toList)
    for (nodeType <- nodeTypes) {
      val pairs = humanPredictionsVsCorrect.filter(_._3.nodeType eq nodeType).map(quad => (quad._1, quad._2))
      printConfusion(hcuPrefix+"overall_confusion_nodetype_"+nodeTypes.indexOf(nodeType)+".csv",
        nodeType.possibleValues.toList,
        pairs)
      printAccuracy(hcuPrefix+"overall_accuracy_nodetype_"+nodeTypes.indexOf(nodeType)+".txt", pairs)
      printExamples(hcuPrefix+"overall_examples_nodetype_"+nodeTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.map(quad => (quad._1, quad._2, quad._3)))
      printExamples(hcuPrefix+"overall_correct_examples_nodetype_"+nodeTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(q => q._1 == q._2).map(quad => (quad._1, quad._2, quad._3)))
      printExamples(hcuPrefix+"overall_incorrect_examples_nodetype_"+nodeTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(q => q._1 != q._2).map(quad => (quad._1, quad._2, quad._3)))
    }
    // Do individual HCU level analysis
    for (hcu <- hcusInvolved) {
      val thisHcuPrefix = hcuPrefix+"/"+hcu.getName+"/"
      val thisHcuReportFolder = new File(thisHcuPrefix)
      if (!thisHcuReportFolder.exists()) thisHcuReportFolder.mkdirs()

      for (nodeType <- nodeTypes) {
        val pairs = humanPredictionsVsCorrect.filter(_._3.nodeType eq nodeType).filter(_._4 eq hcu).map(quad => (quad._1, quad._2))
        printConfusion(thisHcuPrefix+"confusion_nodetype_"+nodeTypes.indexOf(nodeType)+".csv",
          nodeType.possibleValues.toList,
          pairs)
        printAccuracy(thisHcuPrefix+"accuracy_nodetype_"+nodeTypes.indexOf(nodeType)+".txt", pairs)
        printExamples(thisHcuPrefix+"examples_nodetype_"+nodeTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(_._4 eq hcu).map(quad => (quad._1, quad._2, quad._3)))
        printExamples(thisHcuPrefix+"correct_examples_nodetype_"+nodeTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(_._4 eq hcu).filter(q => q._1 == q._2).map(quad => (quad._1, quad._2, quad._3)))
        printExamples(thisHcuPrefix+"incorrect_examples_nodetype_"+nodeTypes.indexOf(nodeType)+".txt", humanPredictionsVsCorrect.filter(_._4 eq hcu).filter(q => q._1 != q._2).map(quad => (quad._1, quad._2, quad._3)))
      }

      frequencyLinePlot(thisHcuPrefix+"latency_curve", "latency", l.flatMap(_._4.humanQueryDelays.filter(_._1 eq hcu).map(_._2.asInstanceOf[Double])).toList)
      dumpRawNumbers(thisHcuPrefix+"latency_data.txt", l.flatMap(_._4.humanQueryDelays.filter(_._1 eq hcu).map(_._2.asInstanceOf[Double])).toList)
    }

    // First we print an overall HCU confusion matrix and accuracy etc

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
    def plotAgainstQueries(xLabel : String, timeSeriesData : Array[Double]) = {
      val plot = new GNUPlot
      plot.addLine(queries, timeSeriesData)
      plot.addLine(queries, smoothTimeSeries(timeSeriesData, 30))
      plot.addLine(queries, smoothTimeSeries(timeSeriesData, 100))
      plot.title = xLabel+" vs time"
      plot.yLabel = xLabel
      plot.xLabel = "time"
      for (idx <- hotSwapIndexes) {
        plot.addVerticalLine(idx)
      }
      plot.saveAnalysis(resultsPrefix+outputPath+"/"+xLabel.replaceAll(" ","_")+"_plot")
    }
    plotAgainstQueries("loss per token", l.map(quad => quad._4.loss / quad._1.nodes.size).toArray)
    plotAgainstQueries("cost per token", l.map(quad => quad._4.requestCost / quad._1.nodes.size).toArray)
    plotAgainstQueries("queries per token", l.map(quad => quad._4.numRequests.asInstanceOf[Double] / quad._1.nodes.size).toArray)
    plotAgainstQueries("delay per token", l.map(quad => quad._4.timeRequired.asInstanceOf[Double] / quad._1.nodes.size).toArray)
    plotAgainstQueries("prior min confidence", l.map(quad => quad._4.initialMinConfidence).toArray)
    plotAgainstQueries("prior max confidence", l.map(quad => quad._4.initialMaxConfidence).toArray)
    plotAgainstQueries("prior avg confidence", l.map(quad => quad._4.initialAvgConfidence).toArray)
    plotAgainstQueries("model training loss", l.map(quad => quad._4.modelTrainingLoss).toArray)
    plotAgainstQueries("model training loss per example", l.map(quad => quad._4.modelTrainingLoss / quad._4.numExamplesSeen).toArray)
    plotAgainstQueries("accuracy", l.map(quad => {
      var localCorrect = 0.0
      var localIncorrect = 0.0
      for (node <- quad._1.nodes) {
        val trueValue = quad._2(node)
        val guessedValue = quad._3(node)
        if (trueValue == guessedValue) localCorrect += 1
        else localIncorrect += 1
      }
      localCorrect / (localCorrect + localIncorrect)
    }).toArray)


    println("Detailed charts printed to \""+resultsPrefix+outputPath+"\"")
  }

  private def classifyWithRealHumans(graph : Graph, hcuPool : HCUPool) : Promise[(Map[GraphNode, String],PredictionSummary)] = {
    // Just ensure that we have a server up, this will start it if it isn't running yet
    WorkUnitServlet.server
    // Run actual prediction
    lenseEngine.predict(graph, (n, hcu) => getQuestion(n, hcu).getHumanOpinion, hcuPool, lossFunction, maxLossPerNode)
  }

  private def classifyWithArtificialHumans(graph : Graph,
                                           output : Output,
                                           humanErrorDistribution : HumanErrorDistribution,
                                           humanDelayDistribution : HumanDelayDistribution,
                                           rand : Random,
                                           hcuPool : HCUPool) : Promise[(Map[GraphNode, String],PredictionSummary)] = {
    lenseEngine.predict(graph, (n, hcu) => {
      val correct = getCorrectLabel(n, output)

      val promise = Promise[String]()
      val workUnit = ArtificialWorkUnit(promise, humanErrorDistribution.guess(correct, n.nodeType.possibleValues), n, hcuPool)
      hcu.addWorkUnit(workUnit)
      workUnit
    },
    hcuPool,
    lossFunction,
    maxLossPerNode)
  }

  private def toLabeledGraph(input : Input, output : Output) : Graph = {
    val graph = toGraph(input)
    val labels = toGoldGraphLabels(graph, output)
    for (node <- graph.nodes) {
      node.observedValue = labels(node)
    }
    graph
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

case class ArtificialWorkUnit(resultPromise : Promise[String], guess : String, node : GraphNode, hcuPool : HCUPool) extends WorkUnit(resultPromise, node, hcuPool)

class ArtificialComputeUnit(humanErrorDistribution : HumanErrorDistribution, humanDelayDistribution : HumanDelayDistribution, workUnitCost : Double, rand : Random, index : Int) extends HumanComputeUnit {
  // Gets the estimated required time to perform this task, in milliseconds
  override def estimateRequiredTimeToFinishItem(node : GraphNode): Long = 1000

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
case class GraphNodeQuestion(questionToDisplay : String, possibleAnswers : List[GraphNodeAnswer], hcu : HumanComputeUnit, node : GraphNode) {
  def getHumanOpinion : WorkUnit = {
    val p = Promise[String]()
    val workUnit = new MulticlassQuestion(
      questionToDisplay,
      possibleAnswers.map(possibleAnswer => (possibleAnswer.displayText, possibleAnswer.internalClassName)),
      p,
      node
    )
    hcu.addWorkUnit(workUnit)
    workUnit
  }
}
