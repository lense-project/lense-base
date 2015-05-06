package edu.stanford.lense_base

import edu.stanford.lense_base.gameplaying.{LookaheadOneHeuristic, GamePlayer}
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
abstract class LenseUseCase[Input <: AnyRef, Output <: AnyRef] {

  lazy val graphStream : GraphStream = new GraphStream()
  lazy val lenseEngine : LenseEngine = new LenseEngine(graphStream, gamePlayer)

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
  def gamePlayer : GamePlayer = LookaheadOneHeuristic

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
                               startNumArtificialHumans : Int) : Unit = {
    val rand = new Random()
    val hcuPool = ArtificialHCUPool(startNumArtificialHumans, humanErrorRate, humanDelayMean, humanDelayStd, workUnitCost, rand)
    analyzeOutput(goldPairs.map(pair => {
      val graph = toGraph(pair._1)
      val goldMap = toGoldGraphLabels(graph, pair._2)
      for (node <- graph.nodes) {
        if (!goldMap.contains(node)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithArtificialHumans(graph, pair._2, humanErrorRate, humanDelayMean, humanDelayStd, rand, hcuPool).future, 1000 days)
      renderClassification(graph, goldMap, guessMap)
      (graph, goldMap, guessMap)
    }))
  }

  /**
   * This will run a test against real live humans. The LenseUseCase needs to be triggered from inside Jetty, once it's
   * running, so that there's a server to ask for human opinions.
   *
   * @param goldPairs pairs of Input and the corresponding correct Output objects
   */
  def testWithRealHumans(goldPairs : List[(Input, Output)]) : Unit = {
    analyzeOutput(goldPairs.map(pair => {
      val graph = toGraph(pair._1)
      val goldMap = toGoldGraphLabels(graph, pair._2)
      for (node <- graph.nodes) {
        if (!goldMap.contains(node)) throw new IllegalStateException("Can't have a gold graph not built from graph's actual nodes")
      }
      val guessMap = Await.result(classifyWithRealHumans(graph, RealHumanHCUPool).future, 1000 days)
      renderClassification(graph, goldMap, guessMap)
      (graph, goldMap, guessMap)
    }))
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
        promise.complete(Try { toOutput(graphAndQuestion, t.get) })
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
  private def analyzeOutput(l : List[(Graph, Map[GraphNode, String], Map[GraphNode, String])]) : Unit = {
    val confusion : mutable.Map[(String,String), Int] = mutable.Map()
    var correct = 0.0
    var incorrect = 0.0
    for (triple <- l) {
      for (node <- triple._1.nodes) {
        val trueValue = triple._2(node)
        val guessedValue = triple._3(node)
        if (trueValue == guessedValue) correct += 1
        else incorrect += 1
        confusion.put((trueValue, guessedValue), confusion.getOrElse((trueValue, guessedValue), 0) + 1)
      }
    }
    println("Accuracy: "+(correct/(correct+incorrect)))
    println("Confusion: "+confusion)
  }

  private def classifyWithRealHumans(graph : Graph, hcuPool : HCUPool) : Promise[Map[GraphNode, String]] = {
    lenseEngine.predict(graph, (n, hcu) => getQuestion(n, hcu).getHumanOpinion, hcuPool, lossFunction)
  }

  private def classifyWithArtificialHumans(graph : Graph,
                                           output : Output,
                                           humanErrorRate : Double,
                                           humanDelayMean : Int,
                                           humanDelayStd : Int,
                                           rand : Random,
                                           hcuPool : HCUPool) : Promise[Map[GraphNode, String]] = {
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
      val workUnit = ArtificialWorkUnit(promise, guess)
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
  for (i <- 0 to startNumHumans) addHCU(new ArtificialComputeUnit(humanErrorRate, humanDelayMean, humanDelayStd, workUnitCost, rand))

  // constantly be randomly adding and removing artificial HCU's
  new Thread {
    override def run() = {
      while (true) {
        Thread.sleep(2000 + Math.round(rand.nextGaussian() * 1000).asInstanceOf[Int])

        // at random, remove a human
        if (rand.nextBoolean() && hcuPool.size > 0) {
          removeHCU(hcuPool.toList(rand.nextInt(hcuPool.size)))
        }
        // or add a human
        else {
          addHCU(new ArtificialComputeUnit(humanErrorRate, humanDelayMean, humanDelayStd, workUnitCost, rand))
        }
      }
    }
  }.start()
}

case class ArtificialWorkUnit(resultPromise : Promise[String], guess : String) extends WorkUnit(resultPromise)

class ArtificialComputeUnit(humanErrorRate : Double, humanDelayMean : Int, humanDelayStd : Int, workUnitCost : Double, rand : Random) extends HumanComputeUnit {
  // Gets the estimated required time to perform this task, in milliseconds
  override def estimateRequiredTimeToFinishItem(workUnit: WorkUnit): Long = humanDelayMean

  // Cancel the current job
  override def cancelCurrentWork(): Unit = {
    // This is a no-op
  }

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
            artificial.resultPromise.complete(Try { artificial.guess })
          }
        }.start()
      }
    }
  }
}

case class GraphNodeAnswer(displayText : String, internalClassName : String)
case class GraphNodeQuestion(questionToDisplay : String, possibleAnswers : List[GraphNodeAnswer], hcu : HumanComputeUnit) {
  def getHumanOpinion : WorkUnit = {
    val p = Promise[String]()
    val workUnit = new MulticlassQuestion(
      questionToDisplay,
      possibleAnswers.map(possibleAnswer => (possibleAnswer.displayText, possibleAnswer.internalClassName)),
      p
    )
    hcu.addWorkUnit(workUnit)
    workUnit
  }
}
