package org.conbere.irc

import Messages._

trait ClassicBot extends Bot {
  val serverName:String
  val nickName:String
  val userName:String
  val password:String
  val realName:String

  val hostName = java.net.InetAddress.getLocalHost.getHostName

  override val onConnect =
    Some(Pass(password) +
         Nick(nickName) +
         User(userName, hostName, serverName, realName))
}

