package org.conbere.irc

import java.net.InetSocketAddress
import ControlChars._
import Tokens._

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

  def address = new InetSocketAddress(domainName, port)

  object Commands {
    def pass =
      Message(None,
              Command("PASS"),
              List(password))

    def nick =
      Message(None,
              Command("NICK"),
              List(nickName))

    def user =
      Message(None,
              Command("USER"),
              List(userName,
                   hostName, 
                   domainName,
                   realName))


    def join(rooms:List[Room]) = {
      // concat all the rooms with keys together and their keys
      val (roomsK:String, keys:String) = rooms.foldLeft(("", "")) {
        case (("", keys), Room(room, Some(key))) =>
          (room, key)
        case ((rooms, keys), Room(room, Some(key))) =>
          (rooms + "," + room, keys + "," + key)
        case (acc, _) =>
          acc
      }

      // concat all the rooms without keys together
      val roomsN = rooms.foldLeft(roomsK) {
        case ("", Room(room, None)) =>
          room
        case (acc, Room(room, None)) =>
          acc + "," + room
        case (acc, _) =>
          acc
      }

      Message(None, Command("JOIN"), List(roomsN, keys))
    }

    def pong(server:String) =
      Message(None, Command("PONG"), List(server))
  }
}

