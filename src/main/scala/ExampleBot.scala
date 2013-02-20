package org.conbere.irc

import Tokens._
import Messages._
import akka.actor._

object Main {
  class Responder extends BotResponder {
    def respondTo(message:Message) = message match {
      case PrivMsg(to, from, text) =>
        println("PRIVMSG: " + to + ", " + from + " " + text)
        None
      case msg@Message(_, _, _) =>
        println(msg)
        None
    }
  }

  def main(args:Array[String]) = {
    val port = 6667
    val system = ActorSystem()
    val rooms = List(Room("#testroom", None))
    val client = new Client( "irc.ny4dev.etsy.com"
                           , 6667
                           , "avibot"
                           , "all_hail_etsy"
                           , "avibot"
                           , "Avi Bot")
    val responder = new Responder
    val server = system.actorOf(Props(new Bot(client, rooms, responder)))
  }
}


