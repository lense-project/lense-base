package edu.stanford.lense_base.server

import java.util.Date

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
object WorkUnitServlet extends ScalatraServlet
  with ScalateSupport with JValueResult
  with JacksonJsonSupport with SessionSupport
  with AtmosphereSupport {

  val workerPool = parallel.mutable.ParHashSet[HCUClient]()

  val workQueue = mutable.Queue[WorkUnit[Any]]()

  def addWorkUnit(workUnit : WorkUnit[Any]) = {
    workQueue.synchronized {
      workQueue.enqueue(workUnit)
      workQueue.notifyAll()
    }
  }

  atmosphere("/work-socket") {
    new HCUClient() {}
  }

  implicit protected def jsonFormats: Formats = org.json4s.DefaultFormats

  // Just for testing

  for (i <- 0 to 10) {
    addWorkUnit(new MulticlassQuestion(
      "<p>Question #"+i+"</p>",
      List("ham", "eggs", "neither"),
      Promise[String]()
    ).asInstanceOf[WorkUnit[Any]])
  }
}

abstract class WorkUnit[T](resultFuture : Promise[T]) {
  def getOutboundMessage : JValue
  def handleReplyMessage(m : JValue) : Boolean
}
case class MulticlassQuestion(questionHTML : String, answers : List[String], resultFuture : Promise[String]) extends WorkUnit[String](resultFuture) {
  override def getOutboundMessage: JValue = {
    new JObject(List(
      "type" -> new JString("multiclass"),
      "html" -> new JString(questionHTML),
      "answers" -> new JArray(answers.map(ans => new JString(ans)))
    ))
  }

  override def handleReplyMessage(m: JValue): Boolean = {

    // If for whatever reason we can't handle this, don't pretend that we did

    try {
      m.asInstanceOf[Map[String,JString]]("Answer").values
    }
    catch {
      case _ : Throwable => return false
    }

    // If we can handle it, then do

    resultFuture.complete(Try {
      m.asInstanceOf[Map[String,JString]]("Answer").values
    })

    true
  }
}

// The Human Compute Unit client
// Handles storing state related to performing tasks
class HCUClient extends AtmosphereClient {

  var currentWork : WorkUnit[Any] = null

  def checkForWork() = {
    WorkUnitServlet.workQueue.synchronized {
      if (WorkUnitServlet.workQueue.nonEmpty) {
        println("Work-queue non-empty. Performing work")
        performWork(WorkUnitServlet.workQueue.dequeue())
      }
      else {
        // wait for new work to arrive
        println("Work-queue empty. Annotator waiting for work")
        WorkUnitServlet.workQueue.wait()
      }
    }
  }

  def performWork(newWork : WorkUnit[Any]) = {
    currentWork = newWork
    println("Doing work "+currentWork)
    send(new JsonMessage(currentWork.getOutboundMessage))
  }

  def returnCurrentWorkUnfinished() = {
    WorkUnitServlet.workQueue.synchronized {
      WorkUnitServlet.workQueue.enqueue(currentWork)
    }
    currentWork = null
  }

  def receive = {
    case Connected =>
      WorkUnitServlet.workerPool += this

      // This should kick off an infinite loop of work-doing, until this object dies

      checkForWork()

    case Disconnected(disconnector, Some(error)) =>
      WorkUnitServlet.workerPool -= this
    case Error(Some(error)) =>
    case TextMessage(text) =>
      send(new TextMessage("ECHO: "+text))

    case m : JsonMessage =>

      // Let our currentWork unit handle the returned value

      if (currentWork != null) {
        val successfullyParsed = currentWork.handleReplyMessage(m.content)
        if (!successfullyParsed) {
          returnCurrentWorkUnfinished()
        }
        send(new JsonMessage(new JObject(List("status" -> JString("success")))))
        checkForWork()
      }

      // This is odd, we're receiving JSON even though we think we have no work outstanding...

      else {
        send(new JsonMessage(new JObject(List("status" -> JString("no_task")))))
      }
  }
}
