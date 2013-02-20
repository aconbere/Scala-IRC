package org.conbere.irc

import java.net.InetSocketAddress
import ControlChars._
import Tokens._
import Messages._

/* I'm not really happy with this class, it kind of represents
  All the necessary config data to initiate a connection
  but that responsibility seems overly broad and weird.
*/
class Client( val domainName:String
            , val port:Int
            , val userName:String
            , val password:String
            , val nickName:String
            , val realName:String) {

  val hostName = java.net.InetAddress.getLocalHost.getHostName
  val address = new InetSocketAddress(domainName, port)
  val pass = Pass(password)
  val nick = Nick(nickName)
  val user = User(userName, hostName, domainName, realName)
}

