package org.conbere.irc

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }

import Tokens._
import ControlChars._

class Bot(client:Client, rooms:List[Room], responder:BotResponder)
extends Actor {

  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)

  def utf8(bytes:ByteString) = bytes.decodeString("UTF-8").trim

  override def preStart {
    println("Connecting to " + client.domainName)
    IOManager(context.system).connect(client.address)
  }

  def readMessage =
    for {
      in <- IO.takeUntil(CRLF)
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

  def respondTo(socket:IO.SocketHandle, message:Message):Unit = {
    message match {
      case Message(_, Command("PING"), List(server)) =>
        println("ping from: " + server)
        socket.write(client.Commands.pong(server).toByteString)
      case Message(_, Command("MODE"), params) =>
        println("Mode set to: " + params)
        socket.write(client.Commands.join(rooms).toByteString)
      case _ =>
        // nothin
    }

    for {
      response <- responder.respondTo(message)
    } {
      socket.write(response.toByteString)
    }
  }

  def receive = {
    case IO.Connected(socket, address) =>
      println("Connected to: " + address)

      login(socket)

      state(socket).flatMap(_ =>
        IO.repeat {
          for {
            parserOutput <- readMessage
          } yield {
            parserOutput match {
              case Parser.Success(message, _) =>
                respondTo(socket, message)
              case _ =>
                println("Could not parse")
            }
          }
        }
      )

    case IO.Read(socket, bytes) =>
      state(socket)(IO.Chunk(bytes))

    case IO.Closed(socket, cause) =>
      println("Closed")
      state(socket)(IO.EOF)
      state -= socket
  }
}

object Main {
  class Responder extends BotResponder {
    def respondTo(message:Message) = message match {
      case msg@Message(prefix, Command("PRIVMSG"), params) =>
        println("PRIVMSG")
        println(msg)
        None
      case msg@Message(_, _, _) =>
        println(msg)
        None
    }
  }

  def main(args:Array[String]) = {
    val port = 6667
    val system = ActorSystem()
    val rooms = List(Room("#testroom", None))
    val client = new Client( "irc.ny4dev.etsy.com"
                           , 6667
                           , "avibot"
                           , "all_hail_etsy"
                           , "avibot"
                           , "opae"
                           , "opae"
                           , "Avi Bot")
    val responder = new Responder
    val server = system.actorOf(Props(new Bot(client, rooms, responder)))
  }
}

