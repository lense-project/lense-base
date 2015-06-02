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
                numAssignments : Int = 1) : String = {
    val title = MTurkConfig.retainerMinutes+" minutes of real time classification with LARGE BONUS"
    val description = "Receive a retainer for staying for "+MTurkConfig.retainerMinutes+" minutes, and a bonus for all the HITs you perform in real time"
    val question = new HITQuestion("src/main/resources/mturk/external.question").getQuestion
    val hit : HIT = service.createHIT(title,
                                      description,
                                      reward,
                                      question,
                                      numAssignments)
    println("Created HIT: "+hit.getHITId)
    println("You can see it here: "+service.getWebsiteURL+"/mturk/preview?groupId="+hit.getHITTypeId)
    hit.getHITId
  }

  def expireHIT(id : String): Unit = {
    System.err.println("Expiring any remaining HITs, now that analysis is complete.")
    service.forceExpireHIT(id)
  }

  def createFixerHIT() : Unit = {
    val title = "Private HIT"
    val description = "DO NOT ACCEPT THIS if I did not contact you personally."
    val question = RequesterService.getBasicFreeTextQuestion("Thanks again for your patients. You can leave this box empty and submit the HIT")
    val hit : HIT = service.createHIT(title,
      description,
      0.00,
      question,
      1)
    println("Created HIT: "+hit.getHITId)
    println("You can see it here: "+service.getWebsiteURL+"/mturk/preview?groupId="+hit.getHITTypeId)
  }

  // This will let me post HITs that I can accept to give bonuses when things get broken
  def main(args : Array[String]) : Unit = {
    createHIT(1.00, 2)
  }
}
