package org.conbere.irc

import akka.util.ByteString

object ControlChars {
  val CRLF = ByteString("\r\n")
  val SP = ByteString(" ")
}
