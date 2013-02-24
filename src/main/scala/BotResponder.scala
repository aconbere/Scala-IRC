package org.conbere.irc

import Messages._
import Tokens._

trait BotResponder {
  val rooms:Option[List[Room]] = None

  val defaultResponse:PartialFunction[Message,Option[Message]] = {
    case Ping(from) =>
      Some(Pong(from))
    case Mode(params) if !rooms.isEmpty =>
      Some(Join(rooms.get))
  }

  val respondTo:PartialFunction[Message,Option[Message]]

  def onConnect:Option[Message] = None

}

