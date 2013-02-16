package org.conbere.irc

import Tokens._

trait BotResponder {
  def respondTo(message:Message):Option[Message]
}

