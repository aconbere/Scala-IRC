import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import akka.util.ByteString

import org.conbere.irc.Parser
import org.conbere.irc.Tokens._
import org.conbere.irc.Messages._

@RunWith(classOf[JUnitRunner])
class TokensSuite extends FunSuite {
  def utf8(bytes:ByteString) =
    bytes.decodeString("UTF-8").trim

  test("produces messages suffixed with :") {
    val userMessage = Message(None, Command("USER"), List("avibot", "opae", "opae", "Avi Bryant"))
    assert(utf8(userMessage.byteString) === "USER avibot opae opae :Avi Bryant")
  }

  test("can add messages together") {
    val m = Pass("password") + User("username", "hostname", "domainname", "real name") + Nick("nickname")
    assert(utf8(m.byteString) === "PASS password\r\nUSER username hostname domainname :real name\r\nNICK nickname")
  }
}
