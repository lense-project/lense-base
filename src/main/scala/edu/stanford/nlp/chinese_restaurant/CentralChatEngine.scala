package edu.stanford.nlp.chinese_restaurant

/**
 * Created by keenon on 5/7/15.
 *
 * Manages piping data around during classification, slotfilling, etc
 */
class CentralChatEngine extends ChatEngine {
  override def receiveMessage(msg: String, sendReply: (String) => Unit): Unit = ???
}
