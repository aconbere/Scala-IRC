package org.conbere.irc

import Tokens._
import Messages._
import akka.actor._
import com.typesafe.scalalogging.log4j.Logging

object Main {
  class Responder extends BotResponder with Logging {
    val respondTo = defaultResponse.orElse[Message,Option[Message]] {
      case PrivMsg(to, from, text) =>
        logger.debug("PRIVMSG: " + to + ", " + from + " " + text)
        None
      case msg@Message(_, _, _) =>
        logger.debug(msg)
        None
    }
  }

  def main(args:Array[String]) = {
    // val rooms = List(Room("#testroom", None))
    Bot.start("irc.test.server.com", 6667, new Responder)
    println("started")
  }
}


