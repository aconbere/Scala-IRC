package org.conbere.irc

import Messages._
import Tokens._

object Implicits {
  implicit class AddableOption(a:Option[Response]) {
    def + (b:Option[Response]) =
      (a,b) match {
        case (None, None) =>None
        case (x@Some(_), None) => x
        case (None, y@Some(_)) => y
        case (Some(x), Some(y)) => Some(x + y)
      }
  }
}

trait Bot {
  import Implicits._

  type MessageHandler = PartialFunction[Message,Option[Response]]
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
    case msg@Mode(params) if !rooms.isEmpty =>
      Some(Join(rooms))
  }

  // helper for building respondTo
  def handleMessage(handler:MessageHandler):MessageHandler = handler

  def and(handlers:MessageHandler*):MessageHandler =
    new PartialFunction[Message, Option[Response]] {
      def apply(m:Message) =
        handlers.foldLeft[Option[Response]](None) {
          (acc, r) => acc + r.lift(m).getOrElse(None)
        }

      def isDefinedAt(m:Message) =
        handlers.find { h => h.isDefinedAt(m) }.nonEmpty
    }
}
