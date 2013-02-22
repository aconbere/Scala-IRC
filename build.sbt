name := "irc"

organization := "org.conbere"

version := "0.1.0"

scalaVersion := "2.10.0"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

testOptions in Test += Tests.Argument("-oDF")

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  } else {
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  }
}

pomIncludeRepository := { _ => false }

licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))

homepage := Some(url("http://github.com/aconbere/scala-irc"))

pomExtra := (
  <scm>
    <url>git@github.com:aconbere/scala-irc.git</url>
    <connection>scm:git:git@github.com:aconbere/scala-irc.git</connection>
  </scm>
  <developers>
    <developer>
      <id>aconbere</id>
      <name>Anders Conbere</name>
      <url>http://anders.conbere.org</url>
    </developer>
  </developers>)
