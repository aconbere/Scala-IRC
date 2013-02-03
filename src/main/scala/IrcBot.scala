package org.conbere.irc

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }
import java.net.InetSocketAddress

object ControlChars {
  val CRLF = ByteString("\r\n")
  val SP = ByteString(" ")
}


object IrcBot {

  def processRequest(socket: IO.SocketHandle): IO.Iteratee[Unit] =
    IO repeat {
      println("repeat")
      IO.repeat {
        for {
          line <- IO.takeUntil(ControlChars.CRLF)
        } yield {
          println(line)
        }
      }
      //} yield {
      //  val rsp = request match {
      //    case Request("GET", "ping" :: Nil, _, _, headers, _) ⇒
      //      OKResponse(ByteString("<p>pong</p>"),
      //        request.headers.exists {
      //          case Header(n, v) ⇒
      //            n.toLowerCase == "connection" && v.toLowerCase == "keep-alive"
      //        })
      //    case req ⇒
      //      OKResponse(ByteString("<p>" + req.toString + "</p>"),
      //        request.headers.exists {
      //          case Header(n, v) ⇒
      //            n.toLowerCase == "connection" && v.toLowerCase == "keep-alive"
      //        })
      //  }
      //  socket write OKResponse.bytes(rsp).compact
      //  if (!rsp.keepAlive) socket.close()
      //}
    }
}

class IrcClient( domainName:String
               , port:Int
               , userName:String
               , password:String
               , nickName:String
               , hostName:String
               , serverName:String
               , realName:String
               ) {
  import ControlChars._

  object Commands {
    def pass =
      (new ByteStringBuilder ++=
           ByteString("PASS") ++=
           SP ++=
           ByteString(password) ++=
           CRLF).result

    def nick =
      (new ByteStringBuilder ++=
           ByteString("NICK") ++=
           SP ++=
           ByteString(nickName) ++=
           CRLF).result

    def user =
      (new ByteStringBuilder ++=
           ByteString("USER") ++=
           SP ++=
           ByteString(userName) ++=
           SP ++=
           ByteString(hostName) ++=
           SP ++=
           ByteString(serverName) ++=
           SP ++=
           ByteString(":" + realName) ++=
           CRLF).result

    def join(rooms:List[(String, Option[String])],
      val grouped = rooms.groupBy { r =>
        r match {
          case (name, None) => false
          case _ => true
        }
      }

      grouped.getOrElse(true, List())

      (new ByteStringBuilder ++=
           ByteString("JOIN")).result

    def pong =
      (new ByteStringBuilder ++= ByteString("PONG")).result
  }

}

class IrcBot( domainName:String
            , port:Int
            , username:String
            , password:String
            , nickName:String
            , hostName:String
            , serverName:String
            , realName:String
            ) extends Actor {

  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)

  val client = new IrcClient( domainName:String
                            , port:Int
                            , username:String
                            , password:String
                            , nickName:String
                            , hostName:String
                            , serverName:String
                            , realName:String)

  def utf8(bytes:ByteString) = bytes.decodeString("UTF-8").trim

  override def preStart {
    println("Connecting to " + domainName)
    val address = new InetSocketAddress(domainName, port)
    IOManager(context.system).connect(address)
  }

  def readMessage =
    for {
      in <- IO.takeUntil(ControlChars.CRLF)
    } yield {
      IrcParser.apply(utf8(in))
    }

  def receive = {
    case IO.Connected(socket, address) =>
      println("Connected")

      // login
      socket.write(client.Commands.pass)
      socket.write(client.Commands.nick)
      socket.write(client.Commands.user)

      for (room <- rooms) {
        socket.write(client.join(room))
      }

      state(socket).flatMap(_ =>
        IO.repeat {
          for {
            message <- readMessage
          } yield {
            message match {
              case IrcParser.Success(Message(_, Command("PING"), List(server)), _) =>
                println("ping from " + server)
                socket.write(client.Commands.pong)
              case _ =>
                println(message)
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

object Bot {
  def main(args:Array[String]) = {
    val port = 6667
    val system = ActorSystem()
    val server = system.actorOf(Props(
      new IrcBot( "irc.ny4dev.etsy.com"
                , 6667
                , "aconbere"
                , "#EndoMorphism!"
                , "aconbot"
                , "opae"
                , "opae"
                , "Anders Conbere"
                , List("etsynomics")
                )))
  }
}

