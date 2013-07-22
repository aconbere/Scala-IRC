package org.conbere.irc

import Tokens.Message
import Messages._
import akka.actor._

import scala.concurrent.duration._
import com.typesafe.scalalogging.log4j.Logging
import scala.language.postfixOps
import akka.actor.ActorSystem

class ExampleBot(
  val serverName:String,
  val nickName:String,
  val userName:String,
  val password:String,
  val realName:String,
  val rooms:List[Room]
) extends ClassicBot with Logging {
  import Implicits._

  def before:Receive = {
    case PrivMsg(from, `nickName`, text) =>
      sender ! PrivMsg(from, text)
  }

  def after:Receive = {
    case Ping(from) =>
      sender ! PrivMsg(from, "hey")
  }

  def receive = onConnect orElse
                defaultHandler orElse
                before orElse after
}

object Main extends Logging {

  def main(args:Array[String]) = {
    val rooms = List(Room("#testroom", None))

    val serverName = "irc.server.com"
    val port = 6667

    val system = ActorSystem("Irc")

    val botProps =
      Props(classOf[ExampleBot], serverName, "testbot", "testbot", "password", "Test Bot", rooms)

    val bot = system.actorOf(botProps)

    val clientProps =
      Props(classOf[Client], serverName, port, bot)

    val client = system.actorOf(clientProps)
  }
}
