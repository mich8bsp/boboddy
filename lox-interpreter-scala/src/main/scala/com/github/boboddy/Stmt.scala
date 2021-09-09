package com.github.boboddy

sealed trait Stmt

case class PrintStmt(expr: Expr) extends Stmt
case class ExpressionStmt(expr: Expr) extends Stmt
