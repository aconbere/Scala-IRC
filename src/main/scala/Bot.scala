package org.conbere.irc

import Messages._
import Tokens._

trait Bot {
  val rooms:List[Room] = List()
  val respondTo:PartialFunction[Message,Option[Response]]
  val onConnect:Option[Response] = None

  val defaultResponse:PartialFunction[Message,Option[Response]] = {
    case Ping(from) =>
      Some(Pong(from))
    case msg@Mode(params) if !rooms.isEmpty =>
      Some(Join(rooms))
  }
}
