package org.conbere.irc

object Implicits {
  import Tokens._
  import Messages._

  implicit class AddableOption(a:Option[Response]) {
    def + (b:Option[Response]):Option[Response] =
      (a,b) match {
        case (None, None) =>None
        case (x@Some(_), None) => x
        case (None, y@Some(_)) => y
        case (Some(x), Some(y)) => Some(x + y)
      }
  }
}

