package org.conbere.irc

import java.net.InetSocketAddress

class Client( val domainName:String
            , val port:Int
            , val userName:String
            , val password:String
            , val nickName:String
            , val hostName:String
            , val serverName:String
            , val realName:String) {

  import ControlChars._
  import Tokens._

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
                   serverName,
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

