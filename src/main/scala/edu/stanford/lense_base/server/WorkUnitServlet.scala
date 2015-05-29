package edu.stanford.lense_base.server

import java.util.Date

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.util.PropertiesClientConfig
import edu.stanford.lense_base.LenseEngine
import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.humancompute.{HCUPool, HumanComputeUnit, WorkUnit}
import edu.stanford.lense_base.model.ModelVariable
import edu.stanford.lense_base.mturk.{MTurkDBState, MTurkDatabase}

import org.json4s._
import org.json4s.jackson.Json
import org.scalatra._
import org.scalatra.atmosphere._
import org.scalatra.json.{JValueResult, JacksonJsonSupport}
import org.scalatra.scalate.ScalateSupport

import scala.collection.parallel
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Promise, Future}
import scala.util.Try

import scala.util.parsing.json.JSONObject

/**
 * Created by keenon on 4/30/15.
 *
 * A Websocket implementation for asking questions and receiving answers.
 */
object WorkUnitServlet {
  val workerPool = parallel.mutable.ParHashSet[HCUClient]()

  var engine : LenseEngine = null

  val workQueue = mutable.Queue[WebWorkUnit]()

  lazy val server = new JettyStandalone("src/main/lense-webapp")

  lazy val service : RequesterService = new RequesterService(new PropertiesClientConfig("/home/keenon/.aws/mturk.properties"))

  lazy val workerIdConnectionMap = mutable.Map[String,HCUClient]()

  var waiting = false
  var waitingForNum = 0

  def waitForSimultaneousConnections(n : Int) : Unit = {
    waiting = true
    waitingForNum = n
    while (true) {
      try {
        val workersConnected = workerIdConnectionMap.count(_._2.workerAccepted)
        if (workersConnected >= n) {
          waiting = false
          return
        }
      }
      catch {
        case e : Throwable => e.printStackTrace()
      }
      System.err.println("Waiting for "+n+" people, so far "+workerIdConnectionMap.count(_._2.workerAccepted))
      for (hcu <- workerPool) {
        hcu.updateWaiting()
      }
      workerIdConnectionMap.synchronized {
        System.err.println("Waiting on workerIdConnectionMap")
        workerIdConnectionMap.wait()
        System.err.println("Received wakeup for workerIdConnectionMap")
      }
    }
    System.err.println("Done waiting for "+n+", starting...")
    waiting = false
  }

  def claimWorkerIdIfPossible(client : HCUClient, workerId : String) : Boolean = workerIdConnectionMap.synchronized {
    if (!workerIdConnectionMap.contains(workerId)) {
      System.err.println("Claiming "+workerId)
      workerIdConnectionMap.put(workerId, client)
      workerIdConnectionMap.notifyAll()
      true
    }
    else {
      false
    }
  }

  def hasWorkerId(client : HCUClient, workerId : String) : Boolean = workerIdConnectionMap.synchronized {
    if (workerIdConnectionMap.contains(workerId)) {
      workerIdConnectionMap(workerId) eq client
    }
    else false
  }

  def releaseWorkerId(client : HCUClient, workerId : String) = workerIdConnectionMap.synchronized {
    if (workerIdConnectionMap.contains(workerId)) {
      if (workerIdConnectionMap(workerId) eq client) {
        workerIdConnectionMap.remove(workerId)
        workerIdConnectionMap.notifyAll()
      }
      else {
        throw new IllegalStateException("Shouldn't be releasing an ID that isn't yours")
      }
    }
  }

  Runtime.getRuntime.addShutdownHook( new Thread(){
    @Override
    override def run(): Unit = {
      for (client <- workerPool) {
        client.completeAndPay()
      }
    }
  })

  def attemptGrantBonus(workerId : String, assignmentId : String, client : HCUClient = null) = {
    // We want to be careful never to double-pay
    MTurkDatabase.synchronized {
      val state: MTurkDBState = MTurkDatabase.getWorker(workerId)
      println("Granting bonus of: " + state.outstandingBonus)
      if (state.outstandingBonus > 0) {
        try {
          // Do the approval
          WorkUnitServlet.service.grantBonus(state.workerId, state.outstandingBonus, assignmentId, "Earned while completing " + state.queriesAnswered + " real-time tasks")
          // Reset all info on this worker
          MTurkDatabase.updateOrCreateWorker(MTurkDBState(workerId, 0, 0L, 0.0, currentlyConnected = false, ""))
        }
        catch {
          case t: Throwable => t.printStackTrace()
        }
      }
    }
  }
}

class WorkUnitServlet extends ScalatraServlet
  with ScalateSupport with JValueResult
  with JacksonJsonSupport with SessionSupport
  with AtmosphereSupport {

  def addWorkUnit[T](workUnit : WebWorkUnit) = {
    // Add work unit
    WorkUnitServlet.workQueue.synchronized {
      WorkUnitServlet.workQueue.enqueue(workUnit.asInstanceOf[WebWorkUnit])
      WorkUnitServlet.workQueue.notifyAll()
    }
    // Boot server if we haven't
    WorkUnitServlet.server
  }

  atmosphere("/work-socket") {
    new HCUClient() {}
  }

  implicit protected def jsonFormats: Formats = org.json4s.DefaultFormats

  // Just for testing

  def main(args : Array[String]) : Unit = {
    for (i <- 0 to 3) {
      addWorkUnit(new MulticlassQuestion(
        "<p>Do you like green eggs and ham? #" + i + "/3</p>",
        List(("ham", "H"), ("eggs", "E"), ("neither", "N")),
        Promise[String](),
        null
      ))
    }
  }
}

abstract class WebWorkUnit(resultPromise : Promise[String], variable : ModelVariable) extends WorkUnit(resultPromise, variable, RealHumanHCUPool) {
  def getOutboundMessage : JValue
  def parseReplyMessage(m : JValue) : String
}

case class MulticlassQuestion(questionHTML : String, choices : List[(String,String)], resultPromise : Promise[String], initVar : ModelVariable) extends WebWorkUnit(resultPromise, initVar) {
  override def getOutboundMessage: JValue = {
    new JObject(List(
      "type" -> new JString("multiclass"),
      "html" -> new JString(questionHTML),
      "choices" -> new JArray(choices.map(choice => new JString(choice._1)))
    ))
  }

  override def parseReplyMessage(m: JValue) : String = {
    println("Received: "+m)

    // If for whatever reason we can't handle this, don't pretend that we did

    try {
      m.asInstanceOf[JObject].values("answer").asInstanceOf[String]
    }
    catch {
      case e : Throwable =>
        taskFailed(e)
    }

    // If we can handle it, then do

    val prettyResult = m.asInstanceOf[JObject].values("answer").asInstanceOf[String]
    val matches = choices.filter(_._1 == prettyResult)
    if (matches.size > 1) throw new IllegalStateException("Cannot have more than 1 option with same human display text: Got \""+prettyResult+"\"")
    if (matches.size == 0) throw new IllegalStateException("Cannot have more than no options with same human display text: Got \""+prettyResult+"\"")
    matches.head._2
  }
}

abstract class TrainingQuestion {
  def getEncodedQuestion : JObject
}

case class IntroductionTrainingQuestion(questionHTML : String) extends TrainingQuestion {
  override def getEncodedQuestion: JObject = {
    new JObject(List(
      "type" -> new JString("introduction"),
      "html" -> new JString(questionHTML)
    ))
  }
}

case class TrainingCheatSheet(questionHTML : String) extends TrainingQuestion {
  override def getEncodedQuestion: JObject = {
    new JObject(List(
      "type" -> new JString("cheat-sheet"),
      "html" -> new JString(questionHTML)
    ))
  }
}

case class MulticlassTrainingQuestion(questionHTML : String, choices : List[String], correctAnswer : String, comments : String) extends TrainingQuestion {
  override def getEncodedQuestion: JObject = {
    new JObject(List(
      "type" -> new JString("multiclass"),
      "html" -> new JString(questionHTML),
      "choices" -> new JArray(choices.map(a => new JString(a))),
      "answer" -> new JString(correctAnswer),
      "comments" -> new JString(comments)
    ))
  }
}

object RealHumanHCUPool extends HCUPool {
  var trainingQuestions : List[TrainingQuestion] = List()

  def getTrainingMessage : JObject = new JObject(List(
    "type" -> new JString("training"),
    "examples" -> new JArray(trainingQuestions.map(_.getEncodedQuestion))
  ))
}

// The Human Compute Unit client
// Handles storing state related to performing tasks
class HCUClient extends AtmosphereClient with HumanComputeUnit {
  var assignmentId : String = null
  var workerId : String = null
  var hitId : String = null
  var startTime : Long = 0

  var completed = false
  var bonusGranted = false

  var workerAccepted = false

  def updateWaiting() = {
    val currentNum = WorkUnitServlet.workerIdConnectionMap.count(_._2.workerAccepted)

    if (workerAccepted && currentNum < WorkUnitServlet.waitingForNum && WorkUnitServlet.waiting) {
      try {
        send(new JsonMessage(new JObject(List("status" -> JString("waiting"),
          "here" -> JInt(currentNum),
          "needed" -> JInt(WorkUnitServlet.waitingForNum)))))
      }
      catch {
        case e : Throwable => e.printStackTrace()
      }
    }
  }

  def noteDisconnection() = {
    if (WorkUnitServlet.hasWorkerId(this, workerId)) {
      WorkUnitServlet.releaseWorkerId(this, workerId)
      val state : MTurkDBState = MTurkDatabase.getWorker(workerId)
      if (state == null) {
        MTurkDatabase.updateOrCreateWorker(MTurkDBState(workerId))
      }
      else {
        MTurkDatabase.updateOrCreateWorker(MTurkDBState(workerId,
          state.queriesAnswered,
          state.connectionDuration,
          state.outstandingBonus,
          currentlyConnected = false,
          state.assignmentId))
      }
    }
  }

  def completeAndPay() = this.synchronized {
    if (!completed) {
      completed = true
      noteDisconnection()
      RealHumanHCUPool.removeHCU(this)
      send(new JsonMessage(new JObject(List("completion-code" -> JString(this.hashCode().toString)))))
      // Wait until well after the user has turned in the HIT, then validate automatically, and pay bonus
      grantBonusInNSeconds()
    }
  }

  def acceptWorker() = {
    val state : MTurkDBState = MTurkDatabase.getWorker(workerId)
    if (state != null) {
      MTurkDatabase.updateOrCreateWorker(MTurkDBState(workerId, state.queriesAnswered, state.connectionDuration, state.outstandingBonus, currentlyConnected = true, assignmentId))
    }
    else {
      MTurkDatabase.updateOrCreateWorker(MTurkDBState(workerId, 0, 0L, 0.0, currentlyConnected = true, assignmentId))
    }
    if (!RealHumanHCUPool.hcuPool.contains(this)) {
      RealHumanHCUPool.addHCU(this)
    }

    System.err.println("Accepted worker "+workerId)

    workerAccepted = true

    System.err.println("Notifying workerIdConnectionMap...")
    try {
      WorkUnitServlet.workerIdConnectionMap.synchronized {
        WorkUnitServlet.workerIdConnectionMap.notifyAll()
      }
    }
    catch {
      case e : Throwable => e.printStackTrace()
    }

    send(new JsonMessage(new JObject(List("status" -> JString("success"), "on-call-duration" -> JInt(retainerDuration())))))
  }

  def receive = {
    case Connected =>
      println("Connected on Atmosphere socket")

    case Disconnected(disconnector, errorOption) =>
      println("Got disconnected")
      noteDisconnection()
      RealHumanHCUPool.removeHCU(this)

    case Error(Some(error)) =>
      println("Got error")
      noteDisconnection()
      RealHumanHCUPool.removeHCU(this)

    case TextMessage(text) =>
      send(new TextMessage("ECHO: "+text))

    case m : JsonMessage =>
      val map = m.content.asInstanceOf[JObject].obj.toMap

      if (map.contains("status")) {
        val status = map.apply("status").values.asInstanceOf[String]

        if (status == "ready") {
          startTime = System.currentTimeMillis()
          assignmentId = map.apply("assignmentId").values.asInstanceOf[String]
          workerId = map.apply("workerId").values.asInstanceOf[String]
          if (workerId == null || workerId == "null") {
            workerId = "default"
          }
          hitId = map.apply("hitId").values.asInstanceOf[String]

          // Here we need to run through two possible reasons the worker can't be added to the pool
          // 1 - this is a double connection
          // 2 - we're out of cash

          val canClaim : Boolean = WorkUnitServlet.claimWorkerIdIfPossible(this, workerId)

          // This means that this workerId already has a connection to us... we need to send them away
          if (!canClaim) {
            System.err.println("Attempted double connect for workerId="+workerId)
            send(new JsonMessage(new JObject(List("status" -> JString("failure"), "display" -> JString("ERROR: It appears that "+
              "someone with your workerId is connected and working on a copy of this HIT."+
              " Please close any other copies of this task before trying to create new ones.")))))
          }
          // Otherwise, we're good to go, so let's initialize this worker
          else {
            // Need to check if sufficient budget to pay worker...
            val haveRetainerBudget = WorkUnitServlet.engine.budget.tryReserveBudget(retainer, this)
            // This means we can't reserve enough budget to pay the retainer, so send away the worker
            if (!haveRetainerBudget) {
              System.err.println("Not enough budget to retain workerId="+workerId)
              send(new JsonMessage(new JObject(List("status" -> JString("failure"), "display" -> JString("ERROR: We don't have "+
                "enough budget left in our job to pay your retainer. Please return this HIT, and check "+
                " back later for more jobs.")))))
            }
            // Finally, we have a clean worker, send them the training set, or accept them immediately if we don't have one
            else {
              if (RealHumanHCUPool.trainingQuestions.size > 0) {
                send(new JsonMessage(RealHumanHCUPool.getTrainingMessage))
              }
              else {
                acceptWorker()
              }
            }
          }
        }
        // TODO: The is majorly NOT hack-proof, someone can easily just send this message to us, and bypass all checks.
        else if (status == "completed-training") {
          acceptWorker()
        }
      }

      else if (map.contains("request") && (System.currentTimeMillis() - startTime > retainerDuration())) {
        val request = map.apply("request").values.asInstanceOf[String]
        if (request == "turn-in") {
          completeAndPay()
        }
      }

      // Let our currentWork unit handle the returned value

      else if (currentWork != null) {
        val replyValue = currentWork.asInstanceOf[WebWorkUnit].parseReplyMessage(m.content)
        val state: MTurkDBState = MTurkDatabase.getWorker(workerId)
        val newState = MTurkDBState(workerId, state.queriesAnswered + 1, System.currentTimeMillis() - startTime, state.outstandingBonus + cost, currentlyConnected = true, assignmentId)
        MTurkDatabase.updateOrCreateWorker(newState)
        send(new JsonMessage(new JObject(List("status" -> JString("success"), "bonus" -> JDouble(newState.outstandingBonus + retainer)))))
        finishWork(currentWork, replyValue)
      }

    case uncaught =>
      println("Something made it through the match expressions: "+uncaught)
  }

  def retainer : Double = {
    // TODO: This needs to be set in a flag someplace
    1.00
  }

  // Gets the estimated required time to perform this task, in milliseconds
  override def estimateRequiredTimeToFinishItem(variable : ModelVariable): Long = {
    // TODO: this needs to be much more sophisticated
    1000
  }

  // Cancel the current job
  override def cancelCurrentWork(): Unit = {
    send(new JsonMessage(new JObject(List(
      "cancelled" -> new JBool(true)
    ))))
    currentWork = null
    workQueue.synchronized {
      // Wake up the work performing code
      workQueue.notify()
    }
  }

  // Get the cost
  override def cost: Double = {
    // TODO: this needs to be set in a flag someplace
    0.01
  }

  // Milliseconds that this worker is expected to remain on call before being paid
  def retainerDuration(): Long = {
    // 30 minutes retainer
    30 * 60 * 1000L
    // 10 second retainer
    // 10 * 1000L
  }

  // Kick off a job
  override def startWork(workUnit: WorkUnit): Unit = {
    this.synchronized {
      val msg = new JsonMessage(workUnit.asInstanceOf[WebWorkUnit].getOutboundMessage)
      println("Sending message: "+msg)
      send(msg)

      new Thread(new Runnable {
        override def run(): Unit = {
          // 10s timeout
          Thread.sleep(10000)
          if (!workUnit.promise.isCompleted) {
            // Timeout the work
            workUnit.revoke()
            cancelCurrentWork()
          }
        }
      }).start()
    }
  }

  def grantBonusInNSeconds() : Unit = {
    println("Waiting 15 seconds to grant bonus")
    this.synchronized {
      if (!bonusGranted) {
        bonusGranted = true
        new Thread(new Runnable {
          override def run(): Unit = {
            Thread.sleep(15000)
            if (assignmentId != null) {
              System.err.println("Attempting to approve assignment")
              WorkUnitServlet.engine.budget.spendReservedBudget(retainer, HCUClient.this)
              WorkUnitServlet.service.approveAssignment(assignmentId, "Good work on real-time tasks")
              System.err.println("Attempting to grant bonus")
              WorkUnitServlet.attemptGrantBonus(workerId, assignmentId, HCUClient.this)
            }
            else {
              System.err.println("Can't grant bonus, no assignmentId")
            }
          }
        }).start()
      }
    }
  }

  override def getName: String = workerId
}
