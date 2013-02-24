package org.conbere.irc

import com.typesafe.scalalogging.log4j.Logging

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }

import Tokens._
import ControlChars._
import Messages._

class Bot ( client:Client, rooms:List[Room], responder:BotResponder)
extends Actor with Logging {
  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)

  def utf8(bytes:ByteString) = bytes.decodeString("UTF-8").trim

  override def preStart {
    logger.debug("Connecting to " + client.domainName)
    IOManager(context.system).connect(client.address)
  }

  def parseMessage(str:String) =
    Parser.apply(str) match {
      case Parser.Success(message, _) =>
        Some(message)
      case _ =>
        logger.error("Could not parse: " + str)
        None
    }

  def readMessage =
    for {
      in <- IO.takeUntil(CRLF)
    } yield {
      parseMessage(utf8(in))
    }

  def login(socket:IO.SocketHandle) = {
    socket.write(client.pass.toByteString)
    socket.write(client.nick.toByteString)
    socket.write(client.user.toByteString)
  }

  def respondTo(socket:IO.SocketHandle, message:Message):Unit = {
    message match {
      case Ping(from) =>
        socket.write(Pong(from).toByteString)
      case Mode(params) =>
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
      logger.debug("Connected to: " + address)

      login(socket)

      state(socket).flatMap(_ =>
        IO.repeat {
          for {
            ioMessage <- readMessage
          } yield {
            for {
              message <- ioMessage
            } yield {
              respondTo(socket, message)
            }
          }
        }
      )

    case IO.Read(socket, bytes) =>
      state(socket)(IO.Chunk(bytes))

    case IO.Closed(socket, cause) =>
      logger.debug("Socket closed")
      state(socket)(IO.EOF)
      state -= socket
  }
}
