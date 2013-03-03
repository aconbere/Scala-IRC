package org.conbere.irc

import akka.util.ByteStringBuilder
import ControlChars._

import Tokens.Message

class ResponseCollection(val responseList:List[Response]=List())
extends Response {
  def +(r:Response) = new ResponseCollection(responseList :+ r )

  val byteString = responseList.foldLeft(new ByteStringBuilder) {
    (acc, m) => acc ++= m.byteString ++= CRLF
  }.result
}
