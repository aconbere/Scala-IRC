package org.conbere.irc

import Tokens.Message
import Messages._
import akka.actor._

import scala.concurrent.duration._
import com.typesafe.scalalogging.log4j.Logging
import scala.language.postfixOps

class ExampleBot( val serverName:String
                , val nickName:String
                , val userName:String
                , val password:String
                , val realName:String
                , val rooms:List[Room])
extends ClassicBot with Logging {
  import Implicits._

  val before = handleMessage {
    case PrivMsg(from, `nickName`, text) =>
      Some(PrivMsg(from, text))
    case PrivMsg(from, to, text) =>
      None
  }

  val after = handleMessage {
    case Ping(from) =>
      Some(PrivMsg("#chan", "hey"))
  }

  val respondTo = defaultResponse + before + after

  override val tickConfig = Some(new TickConfig(1 second, 100 milliseconds))

  override def tick() = Some(PrivMsg("#chan", "Hi!"))
}

object Main extends Logging {

  def main(args:Array[String]) = {
    val rooms = List(Room("#testroom", None))

    val server = "irc.server.com"
    val port = 6667

    val bot = new ExampleBot(server,
                             "testbot",
                             "testbot",
                             "password",
                             "Test Bot",
                             rooms)

    val actor = Client.start(server, port, bot)
  }
}
