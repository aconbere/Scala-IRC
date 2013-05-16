package org.conbere.irc

import java.net.InetSocketAddress
import com.typesafe.scalalogging.log4j.Logging

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }

import Tokens._
import ControlChars._
import Messages._

import scala.concurrent.ExecutionContext.Implicits.global

sealed abstract trait TickLike

case object Tick extends TickLike

object Client extends Logging {
  def start(serverName:String, port:Int, responder:Bot) = {
    val system = ActorSystem()
    val client = new Client(serverName, port, responder)

    val actor = system.actorOf(Props(client))

    responder.tickConfig match {
      case Some(t) =>
        system.scheduler.schedule(t.initialDelay, t.interval)(actor ! Tick)
      case _ =>
    }

    actor
  }
}

class Client(serverName:String, port:Int, responder:Bot)
extends Actor with Logging {
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

  def writeResponseSocket(socket:IO.SocketHandle)(response:Option[Response]) = {
    for {
      resp <- response
    } {
      println("Response: " + utf8(resp.byteString))
      socket.write((new ByteStringBuilder ++= resp.byteString ++= CRLF).result)
    }
  }

  def receive = {
    case IO.Connected(socket, address) =>
      handle = Some(socket)
      val writeResponse = writeResponseSocket(socket) _

      writeResponse(responder.onConnect)

      state(socket).flatMap(_ =>
        IO.repeat {
          IO.takeUntil(CRLF).map { in =>
            for {
              message <- parseMessage(utf8(in))
              if responder.respondTo.isDefinedAt(message)
            } {
              writeResponse(responder.respondTo(message))
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

    case r:Response =>
      handle.foreach { h => writeResponseSocket(h)(Some(r)) }

    case Tick =>
      handle.foreach { h => writeResponseSocket(h)(responder.tick()) }
  }
}
