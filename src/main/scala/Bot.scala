package org.conbere.irc

import Messages._
import Tokens._

import scala.concurrent.duration.FiniteDuration

class TickConfig(val initialDelay: FiniteDuration,
                 val interval: FiniteDuration)

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

  // Tell the client to send a Tick message to the Bot
  // with initialDelay and interval times
  val tickConfig:Option[TickConfig] = None

  def tick():Option[Response] = None

  // helper for building respondTo
  def handleMessage(handler:MessageHandler):MessageHandler = handler
}
