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

object IrcParser extends RegexParsers {
  override def skipWhitespace = false

  def message = opt(':' ~ prefix ~ space) ~ command ~ params ~ crlf
  def prefix = (servername | nick) ~ opt('!' ~ user) ~ opt('@' ~ host)
  def command = word | number ~ number ~ number
  def space = rep(' ')
  def params: Parser[Any] = space ~> opt(':' ~> trailing | middle ~ params)
  def middle = """[^:][^\s\r\n]+""".r
  def trailing = """[^\r\n]+""".r
  def crlf = """\r\n""".r

  def target:Parser[Any] = to ~ opt(',' ~ target)
  def to = channel | user ~ "@" ~ servername | nick | mask
  def channel =  """[#|&].+""".r
  def servername = host
  def host = """"[a-zA-Z0-9.\-]+""".r
  def nick = """\D\S+""".r
  def mask =  """[#|$].+""".r


  def user = """\S+""".r
  def startsWithColon = """:.+""".r
  def word = """[a-zA-Z]*""".r
  def number = """[0-9]""".r
  def special = """[-\[\]\\`^\{\}]""".r

  def any = """.+""".r

  def apply(input:String) = parseAll(message, input)
}
