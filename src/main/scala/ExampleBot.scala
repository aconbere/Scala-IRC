package org.conbere.irc

import Tokens._
import Messages._
import akka.actor._
import com.typesafe.scalalogging.log4j.Logging

class Responder( val serverName:String
               , val nickName:String
               , val userName:String
               , val password:String
               , val realName:String
               , override val rooms:List[Room])
extends BotResponder with Logging {
  val hostName = java.net.InetAddress.getLocalHost.getHostName

  override val onConnect =
    Some(Pass(password) ++=
         Nick(nickName) ++=
         User(userName, hostName, serverName, realName))

  val respondTo = defaultResponse.orElse[Message,Option[Response]] {
    case PrivMsg(to, from, text) =>
      println(to + ", " + from + ", " + text)
      None
    case msg@Message(_, _, _) =>
      println(msg)
      None
  }
}

object Main extends Logging {

  def main(args:Array[String]) = {
    val rooms = List(Room("#testroom", None),
                     Room("#etsynomics", None))

    val responder = new Responder("irc.ny4dev.etsy.com",
                                  "abcdefg",
                                  "abcdefg",
                                  "all_hail_etsy",
                                  "Test Bot",
                                  rooms)

    val server = Bot.start("irc.ny4dev.etsy.com", 6667, responder)
  }
}


