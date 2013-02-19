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
import akka.util.{ ByteString, ByteStringBuilder }
import Tokens._

object Parser extends RegexParsers {
  override def skipWhitespace = false
  def apply(input:String) = parseAll(message, input)

  lazy val message:Parser[Message] =
    opt(":" ~> prefix <~ space) ~
    command ~
    params ^^ {
    case (prefix~command)~params =>
      Message(prefix, command, params)
  }

  lazy val prefix:Parser[Prefix] =
    (serverName | nick ) ~ opt( '!' ~> user ) ~ opt( '@' ~> serverName ) ^^ {
    case t~u~s => Prefix(t, u, s)
  }

  lazy val command:Parser[Command] =
    ("""[0-9]{3}""".r | word) ^^ (Command(_))

  lazy val space = rep(' ')

  lazy val params =
    space ~> opt(repsep(middle, ' ') ~ (space ~> opt(':' ~> trailing))) ^^ {
    case Some(result) =>
      result match {
        case ps~None => ps
        case ps~Some(tr) => ps :+ tr
      }
    case None =>
      List()
  }

  lazy val middle = not(':') ~> """[^\s\r\n]+""".r
  lazy val trailing = """[^\r\n]+""".r
  lazy val crlf = """\r\n""".r

  lazy val target:Parser[Any] = to ~ opt(',' ~ target)
  lazy val to = channel | user ~ '@' ~ serverName | nick | mask
  lazy val channel:Parser[Channel] =  """[#|&].+""".r ^^ (Channel(_))
  lazy val serverName = host
  
  // have to specifically specify what a host is not because the parser is NOT
  // backtracking. Thus will comsume up to the character that is an invalid
  // host char (like _) and then fail.
  lazy val host = """[a-zA-Z0-9.\-^_\-\[\]\\`]+""".r
  lazy val nick = """(\p{L}|[0-9]|[-_\[\]\\`^\{\}])+""".r

  lazy val mask:Parser[UserMask] =  """[#|$].+""".r ^^ (UserMask(_))
  lazy val letter = """[a-zA-Z]""".r
  lazy val user = nick
  lazy val startsWithColon = """:.+""".r
  lazy val word = """[a-zA-Z]*""".r
  lazy val number = """[0-9]""".r
  lazy val special = """[-\[\]\\`^\{\}]""".r

}
