package org.conbere.irc

import Messages._
import Tokens._

trait Bot {
  // a list of channels to join
  val rooms:List[Room]

  // a Partial function that matches against Message instances
  // and returns Option[Message]
  val respondTo:MessageHandler

  // generally speaking this is something like authenticating to the server
  val onConnect:Option[Response] = None

  // some default behaviors that most bots will want to implement
  val defaultResponse:MessageHandler = {
    case Ping(from) =>
      Some(Pong(from))
    case Mode(params) if !rooms.isEmpty =>
      Some(Join(rooms))
  }

  // a time in milliseconds to wait until calling tick
  val tickInterval:Option[Int] = None

  // called in tickInterval
  def tick(room:Room):Option[Response] = None

  // helper for building respondTo
  def handleMessage(handler:MessageHandler):MessageHandler = handler
}
