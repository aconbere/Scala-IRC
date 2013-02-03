import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import org.conbere.irc.IrcParser

@RunWith(classOf[JUnitRunner])
class IrcParserSuite extends FunSuite {
  val messages =
    List( "PASS secretpasswordhere"
        , "NICK Wiz"
        , ":WiZ NICK Kilroy"
        , "USER guest tolmoon tolsun :Ronnie Reagan"
        , ":testnick USER guest tolmoon tolsun :Ronnie Reagan"
        , "SERVER test.oulu.fi 1 :[tolsun.oulu.fi] Experimental server"
        , ":tolsun.oulu.fi SERVER csd.bu.edu 5 :BU Central Server"
        , "OPER foo bar"
        , "QUIT :Gone to have lunch"
        , "SQUIT tolsun.oulu.fi :Bad Link ?"
        , ":Trillian SQUIT cm22.eng.umd.edu :Server out of control"
        , "JOIN #foobar"
        , "JOIN &foo fubar"
        , "JOIN #foo,&bar fubar"
        , "JOIN #foo,#bar fubar,foobar"
        , "JOIN #foo,#bar"
        , ":WiZ JOIN #Twilight_zone"
        , "PART #twilight_zone"
        , "PART #oz-ops,&group5"
        , "MODE #Finnish +im"
        , "MODE #Finnish +o Kilroy"
        , "MODE #Finnish +v Wiz"
        , "MODE #Fins -s"
        , "MODE #42 +k oulu"
        , "MODE #eu-opers +l 10"
        , "MODE &oulu +b"
        , "MODE &oulu +b *!*@*"
        , "MODE &oulu +b *!*@*.edu"
        , ":MODE WiZ -w"
        , ":Angel MODE Angel +i"
        , "MODE WiZ -o"
        , ":Wiz TOPIC #test :New topic"
        , "TOPIC #test :another topic"
        , "TOPIC #test"
        , "NAMES #twilight_zone,#42"
        , "NAMES"
        , "LIST"
        , "LIST #twilight_zone,#42"
        , ":Angel INVITE Wiz #Dust"
        , "INVITE Wiz #Twilight_Zone"
        , "KICK &Melbourne Matthew"
        , "KICK #Finnish John :Speaking English"
        , ":WiZ KICK #Finnish John"
        , ":Wiz VERSION *.se"
        , "VERSION tolsun.oulu.fi"
        , ":Wiz STATS c eff.org"
        , "STATS m"
        , "LINKS *.au"
        , ":WiZ LINKS *.bu.edu *.edu"
        , "TIME tolsun.oulu.fi"
        , "Angel TIME *.au"
        , "CONNECT tolsun.oulu.fi"
        , ":WiZ CONNECT eff.org 6667 csd.bu.edu"
        , "TRACE *.oulu.fi"
        , ":WiZ TRACE AngelDust"
        , "ADMIN tolsun.oulu.fi"
        , ":WiZ ADMIN *.edu"
        , "KILL David (csd.bu.edu <- tolsun.oulu.fi)"
        )

  test("can parse") {
    messages.foreach { m =>
      val result = IrcParser.apply(m + "\r\n") match {
        case IrcParser.Success(_,_) => true
        case _ => false
      }
      assert(result)
    }
  }
}
