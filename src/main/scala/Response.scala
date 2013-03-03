package org.conbere.irc

import akka.util.ByteString

import Tokens.Message

trait Response {
  def +(r:Response):ResponseCollection
  val byteString:ByteString
}
