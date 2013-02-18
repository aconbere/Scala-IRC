package org.conbere.irc 

import akka.util.{ ByteString, ByteStringBuilder }
import ControlChars._

object Tokens {

  sealed trait Token {
    def outputString:String
    def toByteString() = {
      println("sending: " + outputString)
      (new ByteStringBuilder ++= ByteString(outputString) ++= CRLF).result
    }
  }

  sealed trait SimpleToken extends Token {
    val value:String
    def outputString = value
  }

  def mkParamsString(params:List[String]):String = {
    def inner(ps:List[String], acc:String): String =
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

  def mkPrefixString(prefix:Option[Prefix]) =
    prefix.map { p => p.outputString }.getOrElse("")


  case class Channel(value:String) extends SimpleToken
  case class UserMask(value:String) extends SimpleToken
  case class Command(value:String) extends SimpleToken

  case class Prefix(
    target:String,
    user:Option[String],
    host:Option[String]
  ) extends Token {
    def outputString =
      List(target,
           user.getOrElse(""),
           host.getOrElse("")).mkString(" ")
  }

  case class Message(
    prefix:Option[Prefix],
    command:Command,
    params:List[String]
  ) extends Token {
    def outputString = {
      mkPrefixString(prefix) + command.outputString + " " + mkParamsString(params)
    }
  }
}
