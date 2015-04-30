package edu.stanford.lense_base.server

import java.util.Date

import org.json4s.JsonDSL._
import org.json4s._
import org.scalatra._
import org.scalatra.atmosphere._
import org.scalatra.json.{JValueResult, JacksonJsonSupport}
import org.scalatra.scalate.ScalateSupport

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by keenon on 4/30/15.
 *
 * A dummy little Websocket implementation
 */
class WebsocketsServlet extends ScalatraServlet
  with ScalateSupport with JValueResult
  with JacksonJsonSupport with SessionSupport
  with AtmosphereSupport {

  atmosphere("/chat") {
    new AtmosphereClient {
      def receive = {
        case Connected =>
          send("Hello world! I'm from AJAX land")
        case Disconnected(disconnector, Some(error)) =>
        case Error(Some(error)) =>
        case TextMessage(text) => send("ECHO: " + text)
        case JsonMessage(json) => broadcast(json)
      }
    }
  }

  implicit protected def jsonFormats: Formats = org.json4s.DefaultFormats
}