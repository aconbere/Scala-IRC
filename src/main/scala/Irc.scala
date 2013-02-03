package org.conbere.irc
/*
<message>    ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
<prefix>     ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
<command>    ::= <letter> { <letter> } | <number> <number> <number>
<SPACE>      ::= ' ' { ' ' }
<params>     ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
<middle>     ::= <Any *non-empty* sequence of octets not including SPACE or NUL or CR or LF, the first of which may not be ':'>
<trailing>   ::= <Any, possibly *empty*, sequence of octets not including NUL or CR or LF>
<crlf>       ::= CR LF

<target>     ::= <to> [ "," <target> ]
<to>         ::= <channel> | <user> '@' <servername> | <nick> | <mask>
<channel>    ::= ('#' | '&') <chstring>
<servername> ::= <host>
<host>       ::= see RFC 952 [DNS:4] for details on allowed hostnames
<nick>       ::= <letter> { <letter> | <number> | <special> }
<mask>       ::= ('#' | '$') <chstring>
<chstring>   ::= <any 8bit code except SPACE, BELL, NUL, CR, LF and comma (',')>

<user>       ::= <nonwhite> { <nonwhite> }
<letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
<number>     ::= '0' ... '9'
<special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
<nonwhite>   ::= <any 8bit code except SPACE (0x20), NUL (0x0), CR (0xd), and LF (0xa)>
*/

import scala.util.parsing.combinator.RegexParsers

sealed trait Token
sealed trait PrefixTarget extends Token

object Tokens {
  case class Server(name:String) extends PrefixTarget
  case class Nick(name:String) extends PrefixTarget

  case class User(name:String) extends Token
  case class Channel(name:String) extends Token
  case class UserMask(mask:String) extends Token
  case class Command(name:String) extends Token
  case class Param(value:String) extends Token

  case class Prefix(target:PrefixTarget,
                    user:Option[User],
                    host:Option[Server]) extends Token

  case class Message(prefix:Option[Prefix],
                     command:Command,
                     params:List[Param]) extends Token
}

object Parser extends RegexParsers {
  import Tokens._
  override def skipWhitespace = false

  def message:Parser[Message] =
    opt(':' ~> prefix <~ space) ~
    command ~
    params ^^ {
    case (prefix~command)~params =>
      Message(prefix, command, params)
  }

  def prefix:Parser[Prefix] =
    (serverName | nick) ~
    opt('!' ~> user) ~
    opt('@' ~> serverName) ^^ {
    case (target~user)~host =>
      Prefix(target, user, host)
  }

  def command:Parser[Command] =
    (word | """[0-9]{3}""".r) ^^ (Command(_))

  def space = rep(' ')

  def params =
    space ~> opt(repsep(middle, ' ') ~ (space ~> opt(':' ~> trailing))) ^^ {
    case Some(result) =>
      result match {
        case ps~None => ps
        case ps~Some(tr) => ps :+ tr
      }
    case None =>
      List()
  }

  def middle:Parser[Param] = not(':') ~> """[^\s\r\n]+""".r ^^ (Param(_))
  def trailing:Parser[Param] = """[^\r\n]+""".r ^^ (Param(_))
  def crlf = """\r\n""".r

  def target:Parser[Any] = to ~ opt(',' ~ target)
  def to = channel | user ~ '@' ~ serverName | nick | mask
  def channel:Parser[Channel] =  """[#|&].+""".r ^^ (Channel(_))
  def serverName:Parser[Server] = host ^^ (Server(_))
  def host = """[a-zA-Z0-9.\-]+""".r
  def nick:Parser[Nick] = """\D\S+""".r ^^ (Nick(_))
  def mask:Parser[UserMask] =  """[#|$].+""".r ^^ (UserMask(_))

  def user:Parser[User] = """\S+""".r ^^ (User(_))
  def startsWithColon = """:.+""".r
  def word = """[a-zA-Z]*""".r
  def number = """[0-9]""".r
  def special = """[-\[\]\\`^\{\}]""".r

  def apply(input:String) = parseAll(message, input)
}
