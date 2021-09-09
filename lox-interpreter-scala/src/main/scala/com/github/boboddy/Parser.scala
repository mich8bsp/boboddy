package com.github.boboddy

import TokenType._

import scala.collection.mutable

class Parser(tokens: Seq[Token]) {
  private var current: Int = 0

  def parse(): Seq[Stmt] = try {
    val statements: mutable.Buffer[Stmt] = mutable.Buffer()
    while(!isAtEnd){
      statements.append(statement())
    }
    statements.toSeq
  } catch {
    case _: ParseError => Seq()
  } finally {
    current = 0
  }

  private def statement(): Stmt = {
    if(matchExpr(PRINT)){
      printStatement()
    }else{
      expressionStatement()
    }
  }

  private def printStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    PrintStmt(expr)
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    ExpressionStmt(expr)
  }

  private def expression(): Expr = {
    ternary()
  }

  private def ternary(): Expr = {
    //ternary -> equality ("?") expression (":") expression | equality
    var expr = equality()

    if (matchExpr(QUESTION_MARK)) {
      val leftOperator = previous
      val exprIfTrue = expression()
      if (matchExpr(COLON)) {
        val rightOperator = previous
        val exprIfFalse = expression()
        expr = TernaryExpr(
          left = expr,
          middle = exprIfTrue,
          right = exprIfFalse,
          leftOperator = leftOperator,
          rightOperator = rightOperator
        )
      } else {
        throw error(peek, "Expected : after expression.")
      }
    }

    expr
  }

  private def equality(): Expr = {
    // equality  → comparison ( ( "!=" | "==" ) comparison )* ;
    binaryExpr(comparison, BANG_EQUAL, EQUAL_EQUAL)
  }

  private def comparison(): Expr = {
    //comparison  → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    binaryExpr(term, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)
  }

  private def term(): Expr = {
    //term → factor ( ( "-" | "+" ) factor )* ;
    binaryExpr(factor, MINUS, PLUS)
  }

  private def factor(): Expr = {
    //factor → unary ( ( "/" | "*" ) unary )* ;
    binaryExpr(unary, SLASH, STAR)
  }

  private def unary(): Expr = {
    //unary → ( "!" | "-" ) unary | primary ;
    if (matchExpr(BANG, MINUS)) {
      val operator: Token = previous
      val right: Expr = unary()
      UnaryExpr(operator = operator, right = right)
    } else {
      primary()
    }
  }

  private def primary(): Expr = {
    //primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    if (matchExpr(FALSE)) {
      LiteralExpr(Some(false))
    } else if (matchExpr(TRUE)) {
      LiteralExpr(Some(true))
    } else if (matchExpr(NIL)) {
      LiteralExpr(None)
    } else if (matchExpr(NUMBER, STRING)) {
      LiteralExpr(previous.literal)
    } else if (matchExpr(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      GroupingExpr(expr)
    } else {
      throw error(peek, "Expect expression.")
    }
  }

  private def binaryExpr(subExpression: () => Expr,
                         tokenTypes: TokenType*): Expr = {
    val tokensValidOnlyAsBinary = tokenTypes.filterNot(_ == TokenType.MINUS)
    if(matchExpr(tokensValidOnlyAsBinary: _*)){
      //binary expression is missing left side expression
      subExpression() // read right side expression and discard
      throw error(previous, "Expect expression.")
    }else {
      var expr = subExpression()

      while (matchExpr(tokenTypes: _*)) {
        val operator: Token = previous
        val right: Expr = subExpression()
        expr = BinaryExpr(left = expr, operator = operator, right = right)
      }

      expr
    }
  }

  private def consume(tokenType: TokenType,
                      errorMessage: String): Unit = {
    if (check(tokenType)) {
      advance()
    } else {
      throw error(peek, errorMessage)
    }
  }

  private def error(token: Token, errorMessage: String): ParseError = {
    ErrorHandler.error(token, errorMessage)
    new ParseError
  }

  private def matchExpr(expectedTypes: TokenType*): Boolean = {
    var matched: Boolean = false
    expectedTypes.foreach(expectedType => {
      if (!matched && check(expectedType)) {
        advance()
        matched = true
      }
    })

    matched
  }

  private def synchronize(): Unit = {
    advance()
    var syncSuccess = false
    while (!isAtEnd && !syncSuccess) {
      if (previous.tokenType == SEMICOLON) {
        syncSuccess = true
      } else {
        peek.tokenType match {
          case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN =>
            syncSuccess = true
          case _ => advance()
        }
      }
    }
  }

  private def check(expectedType: TokenType): Boolean = {
    !isAtEnd && (peek.tokenType == expectedType)
  }

  private def advance(): Unit = {
    if (!isAtEnd) {
      current += 1
    }
  }

  private def isAtEnd: Boolean = {
    peek.tokenType == TokenType.EOF
  }

  private def peek: Token = tokens(current)

  private def previous: Token = tokens(current - 1)

}
