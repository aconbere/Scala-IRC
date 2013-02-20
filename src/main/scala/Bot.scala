package org.conbere.irc

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }

import Tokens._
import ControlChars._
import Messages._

class Bot (
  client:Client,
  rooms:List[Room],
  responder:BotResponder
) extends Actor {
  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)

  def utf8(bytes:ByteString) = bytes.decodeString("UTF-8").trim

  override def preStart {
    println("Connecting to " + client.domainName)
    IOManager(context.system).connect(client.address)
  }

  def parseMessage(str:String) =
    Parser.apply(str) match {
      case Parser.Success(message, _) =>
        Some(message)
      case _ =>
        println("Could not parse")
        None
    }

  def readMessage =
    for {
      in <- IO.takeUntil(CRLF)
    } yield {
      val str = utf8(in)
      println("Message: " + str)
      parseMessage(str)
    }

  def login(socket:IO.SocketHandle) = {
    socket.write(client.pass.toByteString)
    socket.write(client.nick.toByteString)
    socket.write(client.user.toByteString)
  }

  def respondTo(socket:IO.SocketHandle, message:Message):Unit = {
    message match {
      case Ping(from) =>
        println("ping from: " + from)
        socket.write(Pong(from).toByteString)
      case Mode(params) =>
        println("Mode set to: " + params)
        socket.write(Join(rooms).toByteString)
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
            ioMessage <- readMessage
          } yield {
            for {
              message <- ioMessage
            } yield {
              println(message)
              respondTo(socket, message)
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
