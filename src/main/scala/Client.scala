package org.conbere.irc

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
}

