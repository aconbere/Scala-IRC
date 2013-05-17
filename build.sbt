import AssemblyKeys._

assemblySettings

name := "irc"

version := "0.3.0"

organization := "org.conbere"

licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))

homepage := Some(url("http://github.com/aconbere/scala-irc"))

scalaVersion := "2.10.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)

fork in run := true

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

testOptions in Test += Tests.Argument("-oDF")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
, "junit" % "junit" % "4.10" % "test"
, "com.typesafe.akka" %% "akka-actor" % "2.1.0"
, "org.apache.logging.log4j" % "log4j-core" % "2.0-beta3"
, "com.typesafe" %% "scalalogging-log4j" % "1.0.1"
, "com.typesafe" % "config" % "1.0.0"
)


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
