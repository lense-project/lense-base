package edu.stanford.lense_base

import java.io.{FileWriter, BufferedWriter, File}

import com.github.keenon.minimalml.GNUPlot
import edu.stanford.lense_base.gameplaying._
import edu.stanford.lense_base.graph.{GraphNode, GraphStream, Graph}
import edu.stanford.lense_base.humancompute.{HumanComputeUnit, HCUPool, WorkUnit}
import edu.stanford.lense_base.server.{RealHumanHCUPool, MulticlassQuestion, WorkUnitServlet, WebWorkUnit}

import scala.collection.mutable
import scala.concurrent.{Await, Promise}
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

  lazy val graphStream : GraphStream = new GraphStream()
  lazy val lenseEngine : LenseEngine = new LenseEngine(graphStream, gamePlayer)

  lazy val ensureWorkServerWithDelay = {
    System.err.println("Starting server")
    WorkUnitServlet.server
    System.err.println("Waiting 5 seconds for you to connect, because some gameplayers will see an empty worker pool and just rip through the data")
    Thread.sleep(5000)
  }

  def initialize() : Unit = {
    // Add the training data as a list of labeled graphs
    lenseEngine.addTrainingData(
      initialTrainingData.map(pair =>
        toLabeledGraph(pair._1, pair._2)
      )
    )
  }

  initialize()

  /**
   * This function takes an Input
   * This must return a graph created in the local GraphStream, and it should create a GraphNodeQuestion for each node
   * in the created graph, in case we want to query humans about it.
   *
   * @param input the input that the graph will represent
   * @return a graph representing the input, and taking labels from the output if it is passed in
   */
  def toGraph(input : Input) : Graph

  /**
   * Returns the correct labels for all the nodes in the graph, given a graph and the corresponding gold output. This
   * is used both for generating initial training data and for doing analysis during testing
   *
   * @param graph the graph we need to attach labels to
   * @param goldOutput the output we expect from the graph labellings
   * @return
   */
  def toGoldGraphLabels(graph : Graph, goldOutput : Output) : Map[GraphNode, String] = {
    graph.nodes.map(n => {
      (n, getCorrectLabel(n, goldOutput))
    }).toMap
  }

  def getCorrectLabel(node : GraphNode, goldOutput : Output) : String
  def getQuestion(node : GraphNode, hcu : HumanComputeUnit) : GraphNodeQuestion

  /**
   * Reads the MAP assignment out of the values object, and returns an Output corresponding to this graph having these
   * values.
   * The keys of the values map will always correspond one-to-one with the nodes of the graph.
   *
   * @param graph the graph, with observedValue's on all the nodes
   * @param values a map corresponding the nodes of the graph with their String labels
   * @return an Output version of this graph
   */
  def toOutput(graph : Graph, values : Map[GraphNode, String]) : Output

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
  def lossFunction(mostLikelyGuesses : List[(GraphNode,String,Double)], cost : Double, ms : Long) : Double

  /**
   * An opportunity to provide some seed data for training the model before the online task begins. This data will
   * be used to train the classifier prior to any testing or online activity
   *
   * @return model seed training data
   */
  def initialTrainingData : List[(Input, Output)] = List()

  /**
   * An opportunity to provide a new game player, besides the default
   *
   * @return a game player
   */
  def gamePlayer : GamePlayer = ThresholdHeuristic

  /**
   * A hook to be able to render intermediate progress during testWith[...] calls. Intended to print to stdout.
   */
  def renderClassification(graph : Graph, goldMap : Map[GraphNode, String], guessMap : Map[GraphNode, String]) : Unit = {}

  ////////////////////////////////////////////////
  //
  //  These are functions that LenseUseCase provides, assuming the above are correct
  //
  ////////////////////////////////////////////////

  /**
   * This will run a test against artificial humans, with a "probability epsilon choose uniformly at random" error
   * function. It will print results to stdout.
   *
   * @param goldPairs pairs of Input and the corresponding correct Output objects
   * @param humanErrorRate the error rate epsilon, so if 0.3, then with P=0.3 artificial humans will choose uniformly at random
   */
  def testWithArtificialHumans(goldPairs : List[(Input, Output)],
                               humanErrorRate : Double,
                               humanDelayMean : Int,
                               humanDelayStd : Int,
                               workUnitCost : Double,
                               startNumArtificialHumans : Int,
                               saveTitle : String) : Unit = {
    val rand = new Random()
    val hcuPool = ArtificialHCUPool(startNumArtificialHumans, humanErrorRate, humanDelayMean, humanDelayStd, workUnitCost, rand)

    analyzeOutput(goldPairs.map(pair => {
      val graph = toGraph(pair._1)
      val goldMap = toGoldGraphLabels(graph, pair._2)
      for (node <- graph.nodes) {
        if (!goldMap.contains(node)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithArtificialHumans(graph, pair._2, humanErrorRate, humanDelayMean, humanDelayStd, rand, hcuPool).future, 1000 days)
      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)
      renderClassification(graph, goldMap, guessMap._1)
      (graph, goldMap, guessMap._1, guessMap._2)
    }), hcuPool, saveTitle)

    hcuPool.kill()

    System.exit(0)
  }

  def testBaselineForOfflineLabeling(goldPairs : List[(Input, Output)]) = {
    var trainingExamples : List[Graph] = List[Graph]()
    var numSwapsSoFar = 0

    analyzeOutput(goldPairs.map(pair => {
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
      trainingExamples = trainingExamples :+ cloneGraphPair._1

      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)

      val idx = goldPairs.indexOf(pair)
      if (idx < 50 || (idx < 100 && idx % 10 == 0) || (idx < 200 && idx % 20 == 0)  || (idx % 80 == 0)) {
        graphStream.learn(trainingExamples, 10.0)
        numSwapsSoFar += 1
      }

      renderClassification(graph, goldMap, guessMap)
      val loss = 0
      (graph, goldMap, guessMap, PredictionSummary(loss, 0, 0, 0, 0, 0, initialMinConfidence, initialMaxConfidence, initialAverageConfidence, numSwapsSoFar))
    }), null, "offline_baseline")

    System.exit(0)
  }

  def testBaselineForAllHuman(goldPairs : List[(Input, Output)],
                              humanErrorRate : Double,
                              humanDelayMean : Int,
                              humanDelayStd : Int,
                              workUnitCost : Double,
                              startNumArtificialHumans : Int,
                              numQueriesPerNode : Int) : Unit = {
    val rand = new Random()
    val hcuPool = ArtificialHCUPool(startNumArtificialHumans, humanErrorRate, humanDelayMean, humanDelayStd, workUnitCost, rand)

    lenseEngine.gamePlayer = new NQuestionBaseline(numQueriesPerNode)
    lenseEngine.turnOffLearning()

    analyzeOutput(goldPairs.map(pair => {
      val graph = toGraph(pair._1)
      val goldMap = toGoldGraphLabels(graph, pair._2)
      for (node <- graph.nodes) {
        if (!goldMap.contains(node)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithArtificialHumans(graph, pair._2, humanErrorRate, humanDelayMean, humanDelayStd, rand, hcuPool).future, 1000 days)
      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)
      renderClassification(graph, goldMap, guessMap._1)
      (graph, goldMap, guessMap._1, guessMap._2)
    }), hcuPool, "all_human_"+numQueriesPerNode)

    hcuPool.kill()

    System.exit(0)
  }

  /**
   * This will run a test against real live humans. The LenseUseCase needs to be triggered from inside Jetty, once it's
   * running, so that there's a server to ask for human opinions.
   *
   * @param goldPairs pairs of Input and the corresponding correct Output objects
   */
  def testWithRealHumans(goldPairs : List[(Input, Output)]) : Unit = {
    ensureWorkServerWithDelay

    analyzeOutput(goldPairs.map(pair => {
      val graph = toGraph(pair._1)
      val goldMap = toGoldGraphLabels(graph, pair._2)
      for (node <- graph.nodes) {
        if (!goldMap.contains(node)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithRealHumans(graph, RealHumanHCUPool).future, 1000 days)
      System.err.println("*** finished "+goldPairs.indexOf(pair)+"/"+goldPairs.size)
      renderClassification(graph, goldMap, guessMap._1)
      (graph, goldMap, guessMap._1, guessMap._2)
    }), RealHumanHCUPool, "real_human")
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

  // Prints some outputs to stdout that are the result of analysis
  private def analyzeOutput(l : List[(Graph, Map[GraphNode, String], Map[GraphNode, String], PredictionSummary)], hcuPool : HCUPool, outputPath : String = "default") : Unit = {
    var correct = 0.0
    var incorrect = 0.0
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

    val resultsPrefix = "results/"

    val f = new File(resultsPrefix+outputPath)
    if (!f.exists()) f.mkdirs()
    if (!f.isDirectory) {
      f.delete()
      f.mkdir()
    }

    val summaryResultFile = new File(resultsPrefix+outputPath+"/summary.txt")
    if (!summaryResultFile.exists()) summaryResultFile.createNewFile()
    val bw = new BufferedWriter(new FileWriter(summaryResultFile))
    bw.write("Accuracy: "+(correct/(correct+incorrect))+"\n")
    bw.write("Requested task completion percentage: "+(completed / requested)+"\n")
    bw.write("Avg time/token: "+(time / tokens)+"\n")
    bw.write("Avg requests/token: "+(requested / tokens)+"\n")
    bw.write("Avg completed requests/token: "+(completed / tokens)+"\n")
    bw.close()

    val nodeTypes = l.flatMap(_._1.nodes).map(_.nodeType).distinct.toList
    for (nodeType <- nodeTypes) {
      // Get the confusion matrix
      val confusion : mutable.Map[(String,String), Int] = mutable.Map()
      for (quad <- l) {
        for (node <- quad._1.nodes) {
          if (node.nodeType eq nodeType) {
            val trueValue = quad._2(node)
            val guessedValue = quad._3(node)
            confusion.put((trueValue, guessedValue), confusion.getOrElse((trueValue, guessedValue), 0) + 1)
          }
        }
      }
      // Write out to the file
      val confusionResultFile = new File(resultsPrefix+outputPath+"/confusion_nodetype_"+nodeTypes.indexOf(nodeType)+".csv")
      if (!confusionResultFile.exists()) confusionResultFile.createNewFile()
      val cw = new BufferedWriter(new FileWriter(confusionResultFile))
      // Write out header row
      cw.write("COL=GUESS;ROW=GOLD")
      for (guess <- nodeType.possibleValues) {
        cw.write(",")
        cw.write(guess)
      }
      cw.write("\n")

      for (gold <- nodeType.possibleValues) {
        cw.write(gold)
        cw.write(",")
        var j = 0
        for (guess <- nodeType.possibleValues) {
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
        println(idx+": "+thisPrediction.numSwapsSoFar+","+lastPrediction.numSwapsSoFar)
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
    lenseEngine.predict(graph, (n, hcu) => getQuestion(n, hcu).getHumanOpinion, hcuPool, lossFunction)
  }

  private def classifyWithArtificialHumans(graph : Graph,
                                           output : Output,
                                           humanErrorRate : Double,
                                           humanDelayMean : Int,
                                           humanDelayStd : Int,
                                           rand : Random,
                                           hcuPool : HCUPool) : Promise[(Map[GraphNode, String],PredictionSummary)] = {
    lenseEngine.predict(graph, (n, hcu) => {
      val correct = getCorrectLabel(n, output)
      // Do the automatic error generation
      val guess = if (rand.nextDouble() > humanErrorRate) {
        correct
      }
      else {
        // Pick uniformly at random
        n.nodeType.possibleValues.toList(rand.nextInt(n.nodeType.possibleValues.size))
      }

      val promise = Promise[String]()
      val workUnit = ArtificialWorkUnit(promise, guess, n, hcuPool)
      hcu.addWorkUnit(workUnit)
      workUnit
    },
    hcuPool,
    lossFunction)
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

case class ArtificialHCUPool(startNumHumans : Int, humanErrorRate : Double, humanDelayMean : Int, humanDelayStd : Int, workUnitCost : Double, rand : Random) extends HCUPool {
  for (i <- 1 to startNumHumans) addHCU(new ArtificialComputeUnit(humanErrorRate, humanDelayMean, humanDelayStd, workUnitCost, rand))

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
            addHCU(new ArtificialComputeUnit(humanErrorRate, humanDelayMean, humanDelayStd, workUnitCost, rand))
          }
        }
      }
    }.start()
  }
}

case class ArtificialWorkUnit(resultPromise : Promise[String], guess : String, node : GraphNode, hcuPool : HCUPool) extends WorkUnit(resultPromise, node, hcuPool)

class ArtificialComputeUnit(humanErrorRate : Double, humanDelayMean : Int, humanDelayStd : Int, workUnitCost : Double, rand : Random) extends HumanComputeUnit {
  // Gets the estimated required time to perform this task, in milliseconds
  override def estimateRequiredTimeToFinishItem(node : GraphNode): Long = humanDelayMean

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
            val msDelay = Math.max(1000, Math.round(humanDelayMean + (rand.nextGaussian()*humanDelayStd)).asInstanceOf[Int])
            Thread.sleep(msDelay)
            if (workUnit eq currentWork) {
              finishWork(workUnit, artificial.guess)
            }
          }
        }.start()
      }
    }
  }
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
