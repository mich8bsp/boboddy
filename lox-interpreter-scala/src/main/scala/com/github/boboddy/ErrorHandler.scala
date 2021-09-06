package com.github.boboddy

object ErrorHandler {
  var hadError = false

  def error(line: Int, message: String, where: String = ""): Unit = {
    System.err.println(s"[line $line] Error$where: $message")
    hadError = true
  }

  def error(token: Token, message: String): Unit = {
    val where: String = if(token.tokenType == TokenType.EOF){
      " at end"
    }else{
      s" at '${token.lexeme}'"
    }
    error(line = token.line, message = message, where = where)
  }
}

class ParseError extends RuntimeException