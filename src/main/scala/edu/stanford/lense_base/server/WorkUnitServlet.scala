package edu.stanford.lense_base.server

import java.util.Date

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.util.PropertiesClientConfig
import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.humancompute.{HCUPool, HumanComputeUnit, WorkUnit}

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
 * A dummy little Websocket implementation
 */
object WorkUnitServlet {
  val workerPool = parallel.mutable.ParHashSet[HCUClient]()

  val workQueue = mutable.Queue[WebWorkUnit]()

  lazy val server = new JettyStandalone("src/main/lense-webapp")

  lazy val service : RequesterService = new RequesterService(new PropertiesClientConfig("/home/keenon/.aws/mturk.properties"))
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

abstract class WebWorkUnit(resultPromise : Promise[String], node : GraphNode) extends WorkUnit(resultPromise, node, RealHumanHCUPool) {
  def getOutboundMessage : JValue
  def parseReplyMessage(m : JValue) : String
}

case class MulticlassQuestion(questionHTML : String, choices : List[(String,String)], resultPromise : Promise[String], node : GraphNode) extends WebWorkUnit(resultPromise, node) {
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

object RealHumanHCUPool extends HCUPool {
  override def addHCU(hcu : HumanComputeUnit): Unit = {
    WorkUnitServlet.server
    super.addHCU(hcu)
  }
}

// The Human Compute Unit client
// Handles storing state related to performing tasks
class HCUClient extends AtmosphereClient with HumanComputeUnit {
  var assignmentId : String = null
  var workerId : String = null
  var hitId : String = null
  var bonus : Double = 0.0
  var startTime : Long = 0

  def receive = {
    case Connected =>

    case Disconnected(disconnector, errorOption) =>
      println("Got disconnected")
      RealHumanHCUPool.removeHCU(this)

    case Error(Some(error)) =>
      println("Got error")
      RealHumanHCUPool.removeHCU(this)

    case TextMessage(text) =>
      send(new TextMessage("ECHO: "+text))

    case m : JsonMessage =>
      val map = m.content.asInstanceOf[JObject].obj.toMap
      if (map.contains("status")) {
        val status = map.apply("status").values.asInstanceOf[String]
        if (status == "ready") {
          if (!RealHumanHCUPool.hcuPool.contains(this)) {
            RealHumanHCUPool.addHCU(this)
          }
        }
        startTime = System.currentTimeMillis()
        assignmentId = map.apply("assignmentId").values.asInstanceOf[String]
        workerId = map.apply("workerId").values.asInstanceOf[String]
        hitId = map.apply("hitId").values.asInstanceOf[String]

        send(new JsonMessage(new JObject(List("status" -> JString("success"), "on-call-duration" -> JInt(retainerDuration())))))
      }
      else if (map.contains("request") && (System.currentTimeMillis() - startTime > retainerDuration())) {
        val request = map.apply("request").values.asInstanceOf[String]
        if (request == "turn-in") {
          send(new JsonMessage(new JObject(List("completion-code" -> JString(this.hashCode().toString)))))
          // Wait until well after the user has turned in the HIT, then validate automatically, and pay bonus
          grantBonusInNSeconds()
        }
      }

      // Let our currentWork unit handle the returned value

      else if (currentWork != null) {
        val replyValue = currentWork.asInstanceOf[WebWorkUnit].parseReplyMessage(m.content)
        bonus += cost
        send(new JsonMessage(new JObject(List("status" -> JString("success"), "bonus" -> JDouble(bonus)))))
        finishWork(currentWork, replyValue)
      }

    case uncaught =>
      println("Something made it through the match expressions: "+uncaught)
  }

  // Gets the estimated required time to perform this task, in milliseconds
  override def estimateRequiredTimeToFinishItem(node : GraphNode): Long = {
    // TODO: this needs to be much more sophisticated
    1000
  }

  // Cancel the current job
  override def cancelCurrentWork(): Unit = {
    // TODO: send some message to client
  }

  // Get the cost
  override def cost: Double = {
    // TODO: this needs to be set in a flag someplace
    0.01
  }

  // Milliseconds that this worker is expected to remain on call
  def retainerDuration(): Long = {
    10000L
  }

  // Kick off a job
  override def startWork(workUnit: WorkUnit): Unit = {
    this.synchronized {
      val msg = new JsonMessage(workUnit.asInstanceOf[WebWorkUnit].getOutboundMessage)
      println("Sending message: "+msg)
      send(msg)
    }
  }

  def grantBonusInNSeconds() : Unit = {
    println("Waiting 15 seconds to grant bonus")
    new Thread(new Runnable {
      override def run(): Unit = {
        Thread.sleep(15000)
        println("Granting bonus of: "+bonus)
        WorkUnitServlet.service.approveAssignment(assignmentId, "Good work on real-time tasks")
        WorkUnitServlet.service.grantBonus(workerId, bonus, assignmentId, "Earned while completing real-time tasks")
      }
    }).start()
  }
}
