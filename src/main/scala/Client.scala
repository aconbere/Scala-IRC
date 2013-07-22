package org.conbere.irc

import java.net.InetSocketAddress
import com.typesafe.scalalogging.log4j.Logging

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }

import scala.concurrent.ExecutionContext.Implicits.global

import Tokens._
import ControlChars._
import Messages._

case object Connected

class Client(serverName:String, port:Int, responder:ActorRef) extends Actor with Logging {
  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)
  val address = new InetSocketAddress(serverName, port)

  var handle:Option[IO.SocketHandle] = None

  def utf8(bytes:ByteString) =
    bytes.decodeString("UTF-8").trim

  override def preStart {
    println("Connecting to: " + address)
    IOManager(context.system).connect(address)
  }

  def parseMessage(str:String) =
    Parser.apply(str) match {
      case Parser.Success(message, _) =>
        println("Received: " + message)
        Some(message)
      case _ =>
        logger.error("Could not parse: " + str)
        None
    }

  def receive = {
    case IO.Connected(socket, address) =>
      handle = Some(socket)

      responder ! Connected

      state(socket).flatMap(_ =>
        IO.repeat {
          IO.takeUntil(CRLF).map { in =>
            for ( message <- parseMessage(utf8(in))) {
              responder ! message
            }
          }
        }
      )

    case IO.Read(socket, bytes) =>
      state(socket)(IO.Chunk(bytes))

    case IO.Closed(socket, cause) =>
      state(socket)(IO.EOF)
      state -= socket
      handle = None

    case response:Response =>
      handle.foreach { socket =>
        socket.write(
          (new ByteStringBuilder ++= response.byteString ++= CRLF).result
        )
      }
  }
}
