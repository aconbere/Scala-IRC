package org.conbere.irc

import Tokens._
import Messages._
import akka.actor._
import com.typesafe.scalalogging.log4j.Logging

class ExampleBot( val serverName:String
                , val nickName:String
                , val userName:String
                , val password:String
                , val realName:String
                , override val rooms:List[Room])
extends Bot with Logging {
  val hostName = java.net.InetAddress.getLocalHost.getHostName

  override val onConnect =
    Some(Pass(password) ++=
         Nick(nickName) ++=
         User(userName, hostName, serverName, realName))

  val respondTo = defaultResponse.orElse[Message,Option[Response]] {
    case PrivMsg(from, `nickName`, text) =>
      Some(PrivMsg(from, text))
    case PrivMsg(from, to, text) =>
      None
    case _ =>
      None
  }
}

object Main extends Logging {

  def main(args:Array[String]) = {
    val rooms = List(Room("#testroom", None))

    val server = "irc.server.com"
    val port = 6667

    val responder = new ExampleBot(server,
                                   "testbot",
                                   "testbot",
                                   "password",
                                   "Test Bot",
                                   rooms)

    val actor = Client.start(server, port, responder)
  }
}
