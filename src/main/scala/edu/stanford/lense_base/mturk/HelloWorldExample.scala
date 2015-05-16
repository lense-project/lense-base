package edu.stanford.lense_base.mturk

import com.amazonaws.mturk.addon.{BatchItemCallback, HITDataBuffer}
import com.amazonaws.mturk.requester.HIT
import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.util.PropertiesClientConfig

/**
 * Created by keenon on 5/15/15.
 *
 * This is a super rudimentary example request, seems to work
 */
object HelloWorldExample {
  lazy val service : RequesterService = new RequesterService(new PropertiesClientConfig("/home/keenon/.aws/mturk.properties"))

  def createHelloWorld() : Unit = {
    val title = "Click the verbs"
    val description = "Click on the verbs in a provided sentence"
    val reward = 0.03
    val question = RequesterService.getBasicFreeTextQuestion("What is the weather like right now in Seattle?")
    println(question)
    val numAssignments = 1
    val hit : HIT = service.createHIT(title,
                                      description,
                                      reward,
                                      question,
                                      numAssignments)
    println("Created HIT: "+hit.getHITId)
    println("You can see it here: "+service.getWebsiteURL+"/mturk/preview?groupId="+hit.getHITTypeId)
  }

  def retrieveAnswers() : Unit = {
    val hitDataBuffer = new HITDataBuffer()
    service.getResults(hitDataBuffer, new BatchItemCallback {
      override def processItemResult(itemId: scala.Any, succeeded: Boolean, result: scala.Any, itemException: Exception): Unit = {
        println("Retrieved result!: "+result)
      }
    })
  }

  def main(args : Array[String]) : Unit = {
    createHelloWorld()
    retrieveAnswers()
  }
}
