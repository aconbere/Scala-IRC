package org.conbere.irc

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }
import java.net.InetSocketAddress

object ControlChars {
  val CRLF = ByteString("\r\n")
  val SP = ByteString(" ")
}

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

case class Room(name:String, key:Option[String]) {
  val hasKey = !key.isEmpty
}

class Bot(client:Client, rooms:List[Room]) extends Actor {
  import Tokens._
  import ControlChars._

  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)

  def utf8(bytes:ByteString) = bytes.decodeString("UTF-8").trim

  override def preStart {
    println("Connecting to " + client.domainName)
    IOManager(context.system).connect(client.address)
  }

  def readMessage =
    for {
      in <- IO.takeUntil(ControlChars.CRLF)
    } yield {
      val str = utf8(in)
      println("Message: " + str)
      Parser.apply(str)
    }

  def login(socket:IO.SocketHandle) = {
    socket.write(client.Commands.pass.toByteString)
    socket.write(client.Commands.nick.toByteString)
    socket.write(client.Commands.user.toByteString)
  }

  def receive = {
    case IO.Connected(socket, address) =>
      println("Connected to: " + address)

      login(socket)

      state(socket).flatMap(_ =>
        IO.repeat {
          for {
            message <- readMessage
          } yield {
            message match {
              case Parser.Success(Message(_, Command("PING"), List(server)), _) =>
                println("ping from: " + server)
                socket.write(client.Commands.pong(server).toByteString)
              case Parser.Success(Message(_, Command("MODE"), params), _) =>
                println("Mode set to: " + params)
                socket.write(client.Commands.join(rooms).toByteString)
              case org@Parser.Success(msg@Message(prefix, Command("PRIVMSG"), params), _) =>
                println("PRIVMSG")
                println(msg)
              case org@Parser.Success(msg@Message(_, _, _), _) =>
                println(msg)
            }
          }
        }
      )

    case IO.Read(socket, bytes) =>
      state(socket)(IO.Chunk(bytes))

    case IO.Closed(socket, cause) =>
      state(socket)(IO.EOF)
      state -= socket
  }
}

object Main {
  def main(args:Array[String]) = {
    val port = 6667
    val system = ActorSystem()
    val rooms = List(Room("#testroom", None))
    val client = new Client( "server"
                           , 6667
                           , "username"
                           , "password"
                           , "nick"
                           , "hostname"
                           , "servername"
                           , "Anders Irc Bot"
                           )
    val server = system.actorOf(Props(new Bot(client, rooms)))
  }
}

