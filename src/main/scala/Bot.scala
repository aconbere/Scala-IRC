package org.conbere.irc

import akka.actor._
import Messages._
import Tokens._

trait Bot extends Actor {
  // a list of channels to join
  val rooms:List[Room]

  // some default behaviors that most bots will want to implement
  val defaultHandler:Receive = {
    case Ping(from) =>
      sender ! Pong(from)
    case Mode(params) if !rooms.isEmpty =>
      sender ! Join(rooms)
  }
}
