package org.conbere.irc

import java.net.InetSocketAddress
import com.typesafe.scalalogging.log4j.Logging

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }

import Tokens._
import ControlChars._
import Messages._

object Bot {
  def start(serverName:String, port:Int, responder:BotResponder) =
    ActorSystem().actorOf(Props(new Bot(serverName, port, responder)))
}

class Bot(serverName:String, port:Int, responder:BotResponder)
extends Actor with Logging {
  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)
  val address = new InetSocketAddress(serverName, port)
  val hostName = java.net.InetAddress.getLocalHost.getHostName

  def utf8(bytes:ByteString) =
    bytes.decodeString("UTF-8").trim

  override def preStart {
    logger.debug("Connecting to " + serverName)
    IOManager(context.system).connect(address)
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

  def writeResponseSocket(socket:IO.SocketHandle)(message:Option[Message]) =
    message match {
      case Some(message) =>
        socket.write(message.toByteString)
      case _ =>
        // no response
    }

  def receive = {
    case IO.Connected(socket, address) =>
      logger.debug("Connected to: " + address)
      val writeResponse = writeResponseSocket(socket) _

      writeResponse(responder.onConnect)

      state(socket).flatMap(_ =>
        IO.repeat {
          for {
            ioMessage <- readMessage
          } yield {
            for {
              message <- ioMessage
            } yield {
              if (responder.respondTo.isDefinedAt(message)) {
                writeResponse(responder.respondTo(message))
              }
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
