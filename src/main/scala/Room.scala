package org.conbere.irc

case class Room(name:String, key:Option[String]) {
  val hasKey = !key.isEmpty
}
