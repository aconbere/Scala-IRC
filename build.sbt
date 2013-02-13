name := "irc"

version := "0.1.0"

scalaVersion := "2.10.0"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

testOptions in Test += Tests.Argument("-oDF")
