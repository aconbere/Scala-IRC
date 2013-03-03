package org.conbere.irc

import akka.util.ByteString
import Tokens.Message

class NullMessage extends Response {
  def +(r:Response) = new ResponseCollection(List(r))
  val byteString = ByteString("")
}
