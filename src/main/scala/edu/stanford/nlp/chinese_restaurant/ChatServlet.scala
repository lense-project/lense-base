package edu.stanford.nlp.chinese_restaurant

import java.util.Date

import edu.stanford.lense_base.graph.GraphNode
import edu.stanford.lense_base.humancompute.{HCUPool, HumanComputeUnit, WorkUnit}
import edu.stanford.lense_base.server.JettyStandalone
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

object ChatServer {
  def main(args : Array[String]) = {
    new JettyStandalone("src/main/chinese-restaurant-webapp")
  }
}

/**
 * Created by keenon on 5/6/15.
 *
 * Forms the entry point for a chat-bot that talks to L.E.N.S.E. pipelines in the background
 */
class ChatServlet extends ScalatraServlet
  with ScalateSupport with JValueResult
  with JacksonJsonSupport with SessionSupport
  with AtmosphereSupport {

  atmosphere("/chat-socket") {
    new ChatSocket() {}
  }

  implicit protected def jsonFormats: Formats = org.json4s.DefaultFormats
}

abstract class ChatEngine {
  def receiveMessage(msg : String, sendReply : (String) => Unit) : Unit
}

object YourMomChatEngine extends ChatEngine {
  override def receiveMessage(msg: String, sendReply: (String) => Unit): Unit = {
    sendReply("Your mom likes \""+msg+"\"")
  }
}

class ChatSocket extends AtmosphereClient {
  val chatEngine = new CentralChatEngine()

  def sendMessage(msg : String): Unit = {
    System.err.println("Sending message: "+msg)
    send(new JsonMessage(new JObject(List(
      "message" -> msg
    ))))
  }

  def receive = {
    case Connected =>

    case Disconnected(disconnector, errorOption) =>
      println("Got disconnected")

    case Error(Some(error)) =>
      println("Got error")

    case TextMessage(text) =>
      // send(new TextMessage("ECHO: "+text))

    case m : JsonMessage =>
      val map = m.content.asInstanceOf[JObject].obj.toMap
      if (map.contains("status")) {
        val status = map.apply("status").values.asInstanceOf[String]
        if (status == "ready") {
          System.err.println("Got a new chat customer!")
        }
      }
      if (map.contains("message")) {
        val msg = map.apply("message").values.asInstanceOf[String]
        System.err.println("Got new message: "+msg)
        chatEngine.receiveMessage(msg, sendMessage)
      }

    case uncaught =>
      println("Something made it through the match expressions: "+uncaught)
  }
}

