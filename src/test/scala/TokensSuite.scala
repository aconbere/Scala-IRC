import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import akka.util.ByteString

import org.conbere.irc.Parser
import org.conbere.irc.Tokens._

@RunWith(classOf[JUnitRunner])
class TokensSuite extends FunSuite {
  test("produces messages suffixed with :") {
    val userMessage = Message(None, Command("USER"), List("avibot", "opae", "opae", "Avi Bryant"))
    assert(userMessage.byteString === ByteString("USER avibot opae opae :Avi Bryant"))
  }
}
