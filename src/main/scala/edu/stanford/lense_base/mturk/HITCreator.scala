package edu.stanford.lense_base.mturk

import com.amazonaws.mturk.addon.{HITQuestion, BatchItemCallback, HITDataBuffer}
import com.amazonaws.mturk.requester.HIT
import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.util.PropertiesClientConfig

/**
 * Created by keenon on 5/15/15.
 *
 * This is a super rudimentary example request, seems to work
 */
object HITCreator {
  lazy val service : RequesterService = new RequesterService(new PropertiesClientConfig("/home/keenon/.aws/mturk.properties"))

  def createHIT(reward : Double = 0.10,
                numAssignments : Int = 1) : Unit = {
    val title = "Real time classification with LARGE BONUS"
    val description = "Receive a retainer for staying for 10 minutes, and a bonus for all the HITs you perform in real time"
    val question = new HITQuestion("src/main/resources/mturk/external.question").getQuestion
    val hit : HIT = service.createHIT(title,
                                      description,
                                      reward,
                                      question,
                                      numAssignments)
    println("Created HIT: "+hit.getHITId)
    println("You can see it here: "+service.getWebsiteURL+"/mturk/preview?groupId="+hit.getHITTypeId)
  }

  def main(args : Array[String]) : Unit = {
    createHIT()
  }
}
