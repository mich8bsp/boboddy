package com.github.boboddy

object ASTPrinter {

  def print(expr: Expr): String = expr match {
    case BinaryExpr(left, operator, right) =>
      parenthesize(operator.lexeme, left, right)
    case GroupingExpr(expression) =>
      parenthesize("group", expression)
    case LiteralExpr(value) =>
      value.map(_.toString).getOrElse("nil")
    case UnaryExpr(operator, right) =>
      parenthesize(operator.lexeme, right)
  }

  private def parenthesize(name: String, exprs: Expr*): String = {
    s"(${(Seq(name) ++ exprs.map(print)).mkString(" ")})"
  }
}
