package org.conbere.irc

import java.net.InetSocketAddress
import com.typesafe.scalalogging.log4j.Logging

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }

import Tokens._
import ControlChars._
import Messages._

object Bot extends Logging {
  def start(serverName:String, port:Int, responder:BotResponder) = {
    ActorSystem().actorOf(Props(new Bot(serverName, port, responder)))
  }
}

class Bot(serverName:String, port:Int, responder:BotResponder)
extends Actor with Logging {
  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)
  val address = new InetSocketAddress(serverName, port)

  def utf8(bytes:ByteString) =
    bytes.decodeString("UTF-8").trim

  override def preStart {
    println("Connecting to: " + address)
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

  def writeResponseSocket(socket:IO.SocketHandle)(response:Option[Response]) = {
    response match {
      case Some(response) =>
        println("Response: " + utf8(response.byteString))
        socket.write(response.byteString)
      case _ =>
        // no response
    }
  }

  def receive = {
    case IO.Connected(socket, address) =>
      val writeResponse = writeResponseSocket(socket) _

      writeResponse(responder.onConnect)

      state(socket).flatMap(_ =>
        IO.repeat {
          IO.takeUntil(CRLF).map { in =>
            parseMessage(utf8(in)) match {
              case Some(message) =>
                if (responder.respondTo.isDefinedAt(message))
                  writeResponse(responder.respondTo(message))
              case None =>
                // do nothing
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
