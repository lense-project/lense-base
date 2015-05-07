package edu.stanford.lense_base.server

import java.util.Date

import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.humancompute.{HCUPool, HumanComputeUnit, WorkUnit}
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext

import org.atmosphere.interceptor.IdleResourceInterceptor
import org.json4s.JsonDSL._
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

  lazy val server = {
    val server = new Server()
    val connector = new SelectChannelConnector()
    connector.setPort(8080)
    server.addConnector(connector)
    val context: WebAppContext = new WebAppContext("src/main/lense-webapp", "/")
    context.setServer(server)
    server.setHandler(context)

    try {
      server.start()
    } catch {
      case e: Exception => {
        e.printStackTrace()
        System.exit(1)
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

abstract class WebWorkUnit(resultPromise : Promise[String], node : GraphNode) extends WorkUnit(resultPromise, node) {
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
      }

      // Let our currentWork unit handle the returned value

      else if (currentWork != null) {
        val replyValue = currentWork.asInstanceOf[WebWorkUnit].parseReplyMessage(m.content)
        send(new JsonMessage(new JObject(List("status" -> JString("success")))))
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

  // Kick off a job
  override def startWork(workUnit: WorkUnit): Unit = {
    this.synchronized {
      val msg = new JsonMessage(workUnit.asInstanceOf[WebWorkUnit].getOutboundMessage)
      println("Sending message: "+msg)
      send(msg)
    }
  }
}
