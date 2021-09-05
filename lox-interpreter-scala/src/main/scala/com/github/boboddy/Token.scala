package com.github.boboddy

case class Token(
                tokenType: TokenType,
                lexeme: String,
                literal: Option[Object],
                line: Int
                ){

  override def toString: String = s"$tokenType $lexeme ${literal.map(_.toString).getOrElse("")}"
}
