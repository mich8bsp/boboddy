package com.github.boboddy

object ErrorHandler {
  var hadError = false

  def error(line: Int, message: String, where: String = ""): Unit = {
    System.err.println(s"[line $line] Error$where: $message")
    hadError = true
  }
}
