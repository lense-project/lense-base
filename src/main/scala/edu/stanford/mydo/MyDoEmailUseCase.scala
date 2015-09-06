package edu.stanford.mydo

import scalaj.http._
import org.json4s._
import org.json4s.jackson.Json
import org.json4s.native.JsonMethods._

/**
 * Created by keenon on 6/8/15.
 *
 * Periodically connects to the MyDo email update service. Will pipe emails through the parsing process to extract any
 * useful todo items
 */
object MyDoEmailUseCase {
  val GET_EMAILS_URL = "http://powerful-brook-4751.herokuapp.com/mydo/ai/get_emails/"
  val POST_EMAILS_URL = "http://powerful-brook-4751.herokuapp.com/mydo/ai/post_email_items/"

  def main(args : Array[String]) : Unit = {
    new Thread(new Runnable {
      override def run(): Unit = {
        while (true) {
          val emails = retrieveEmails()
          for (email <- emails) {
            parseEmail(email)
          }
          Thread.sleep(1000)
        }
      }
    }).start()
  }

  def retrieveEmails() : List[Map[String,Any]] = {
    val response: HttpResponse[String] = Http(GET_EMAILS_URL).asString
    val json = parse(response.body)
    val emails = json.asInstanceOf[JObject].values("emails").asInstanceOf[List[Map[String,Any]]]
    emails
  }

  def parseEmail(email : Map[String,Any]) = {
    val body = email("body").asInstanceOf[String]
    val subject = email("subject").asInstanceOf[String]
    val to = email("to").asInstanceOf[List[String]]
    val id = email("id").asInstanceOf[BigInt]
    val from = email("from").asInstanceOf[String]

    val obj = Email(body, subject, to, from, id)
    TodoExtractor.extractTodos(obj)
    postEmail(obj)

    println(body)
  }

  def postEmail(email : Email) : Unit = {
    val data = new JObject(List(
      "items" -> new JArray(List(
        JObject(List(
          "id" -> JInt(email.id),
          "headers" -> JArray(List(
            JObject(List(
              "justification" -> JString("In email from "+email.from),
              "title" -> JString("From "+email.from),
              "items" -> JArray(email.todos.toList.map(todo => {
                JObject(List(
                  "done" -> JBool(false),
                  "title" -> JString(todo)
                ))
              }))
            ))
          ))
        ))
      ))
    ))
    println(pretty(render(data)))
    val ret = Http(POST_EMAILS_URL).postData(pretty(render(data)))
      .header("Content-Type", "application/json")
      .header("Charset", "UTF-8")
      .option(HttpOptions.readTimeout(10000))
      .asString
    println("Server returned: "+ret)
  }
}
