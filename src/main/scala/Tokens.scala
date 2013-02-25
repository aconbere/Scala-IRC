package org.conbere.irc 

import akka.util.{ ByteString, ByteStringBuilder }
import ControlChars._

object Tokens {

  sealed trait Token {
    val byteString:ByteString
  }

  sealed trait SimpleToken extends Token {
    val value:String
    val byteString = ByteString(value)
  }

  case class Channel(value:String) extends SimpleToken
  case class UserMask(value:String) extends SimpleToken
  case class Command(value:String) extends SimpleToken


  case class Prefix(target:String, user:Option[String], host:Option[String])
  extends Token {
    val byteString =
      ByteString(List(target,
                 user.getOrElse(""),
                 host.getOrElse("")).mkString(" "))
  }

  trait Response {
    def ++=(msg:Message):MessageCollection
    val byteString:ByteString
  }

  case class Message(prefix:Option[Prefix], command:Command, params:List[String])
  extends Token with Response {
    def ++=(m:Message) = new MessageCollection(List(this, m))

    def mkPrefixByteString(prefix:Option[Prefix]) =
       prefix.map { p => p.byteString }.getOrElse(ByteString(""))
      
    def mkParamsString(params:List[String]) = {
      def inner(ps:List[String], acc:String):String =
        ps match {
          case x :: y :: xs =>
            inner(xs, acc + " " + x + " " + y)
          case x :: Nil =>
            acc + " :" + x
          case Nil =>
            acc
        }

      params match {
        case Nil => ""
        case x :: xs => inner(xs, x)
      }
    }

    val byteString = 
      (new ByteStringBuilder ++=
        mkPrefixByteString(prefix) ++=
        command.byteString ++=
        SP ++= 
        ByteString(mkParamsString(params))).result
  }

  class MessageCollection(val ml:List[Message])
  extends Token with Response {
    def ++=(m:Message) = new MessageCollection(ml :+ m )
    val byteString = ml.foldLeft(new ByteStringBuilder) {
      (acc, m) => acc ++= m.byteString ++= CRLF
    }.result
  }
}
