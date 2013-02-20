package org.conbere.irc

import Tokens._

object Messages {
  object Pong {
    def apply(to:String) =
      Message(None, Command("PONG"), List(to))

    def unapply(msg:Message) = {
      msg match {
        case Message(_,Command("PONG"), List(from)) =>
          Some(from)
        case _ =>
          None
      }
    }
  }

  object Ping {
    def apply(to:String) =
      Message(None, Command("PING"), List(to))

    def unapply(msg:Message) = {
      msg match {
        case Message(_,Command("PING"), List(from)) =>
          Some(from)
        case _ =>
          None
      }
    }
  }

  object PrivMsg {
    def apply(to:String, text:String) =
        Message(None, Command("PRIVMSG"), List(to, text))

    def unapply(msg:Message) = {
      msg match {
        case Message(Some(Prefix(from, _, _)), Command("PRIVMSG"), List(to, text)) =>
          Some((to, from, text))
        case _ =>
          None
      }
    }
  }

  object Mode {
    def apply() =
      Message(None, Command("MODE"), List())

    def unapply(msg:Message) = {
      msg match {
        case Message(_, Command("MODE"), params) =>
          Some(params)
        case _ =>
          None
      }
    }
  }

  object Pass {
    def apply(password:String) =
      Message(None, Command("PASS"), List(password))

    def unapply(msg:Message) = {
      msg match {
        case Message(_, Command("PASS"), List(password)) =>
          Some(password)
        case _ =>
          None
      }
    }
  }

  object Nick {
    def apply(nick:String) =
      Message(None, Command("NICK"), List(nick))

    def unapply(msg:Message) = {
      msg match {
        case Message(_, Command("NICK"), List(nick)) =>
          Some(nick)
        case _ =>
          None
      }
    }
  }

  object User {
    def apply(userName:String, hostName:String, domainName:String, realName:String) =
      Message(None,
              Command("USER"),
              List(userName,
                   hostName, 
                   domainName,
                   realName))

    def unapply(msg:Message) = {
      msg match {
        case Message(_, Command("USER"), List(userName, hostName, domainName, realName)) =>
          Some((userName, hostName, domainName, realName))
        case _ =>
          None
      }
    }
  }

  object Join {
    def apply(rooms:List[Room]) = {
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

    def unapply(msg:Message) = {
      msg match {
        case Message(_, Command("JOIN"), List(rooms, keys)) =>
          Some((rooms, keys))
        case _ =>
          None
      }
    }
  }
}
