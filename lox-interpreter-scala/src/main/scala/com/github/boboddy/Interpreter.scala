package com.github.boboddy

object Interpreter {


  def interpret(expression: Expr): Unit = {
    try {
      val evaluationRes: Option[Any] = evaluate(expression)
      println(stringify(evaluationRes))
    } catch {
      case e: RuntimeError =>
        ErrorHandler.runtimeError(e)
    }
  }

  def evaluate(expression: Expr): Option[Any] = expression match {
    case BinaryExpr(left, operator, right) =>
      val leftValue: Option[Any] = evaluate(left)
      val rightValue: Option[Any] = evaluate(right)
      operator.tokenType match {
        case TokenType.MINUS =>
          val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
          Some(left - right)
        case TokenType.SLASH =>
          val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
          Some(left / right)
        case TokenType.STAR =>
          val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
          Some(left * right)
        case TokenType.PLUS =>
          (leftValue, rightValue) match {
            case (Some(d1: Double), Some(d2: Double)) => Some(d1 + d2)
            case (Some(d1: String), Some(d2: String)) => Some(d1 + d2)
            case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
          }
        case TokenType.LESS =>
          val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
          Some(left < right)
        case TokenType.LESS_EQUAL =>
          val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
          Some(left <= right)
        case TokenType.GREATER =>
          val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
          Some(left > right)
        case TokenType.GREATER_EQUAL =>
          val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
          Some(left >= right)
        case TokenType.EQUAL_EQUAL => Some(leftValue == rightValue)
        case TokenType.BANG_EQUAL => Some(leftValue != rightValue)
        case _ => throw RuntimeError(operator, "Unrecognized binary operator.")
      }
    case GroupingExpr(expression) => evaluate(expression)
    case LiteralExpr(value) => value
    case UnaryExpr(operator, right) =>
      val rightValue: Option[Any] = evaluate(right)
      operator.tokenType match {
        case TokenType.MINUS => Some(-getOperandAsNumber(operator, rightValue))
        case TokenType.BANG => Some(!isTruthy(rightValue))
        case _ => throw RuntimeError(operator, "Unrecognized unary operator.")
      }
    case TernaryExpr(left, leftOperator, middle, rightOperator, right) =>
      (leftOperator.tokenType, rightOperator.tokenType) match {
        case (TokenType.QUESTION_MARK, TokenType.COLON) =>
          val leftValue: Option[Any] = evaluate(left)
          if(isTruthy(leftValue)){
            evaluate(middle)
          }else{
            evaluate(right)
          }
        case _ => throw RuntimeError(leftOperator, "Unrecognized ternary operator.")
      }
  }

  private def isTruthy(exprValue: Option[Any]): Boolean = exprValue match {
    case None | Some(false) => false
    case _ => true
  }

  private def getOperandAsNumber(operator: Token, operand: Option[Any]): Double = {
    operand match {
      case Some(d: Double) => d
      case _ => throw RuntimeError(operator, "Operand must be a number.")
    }
  }

  private def getOperandsAsNumbers(operator: Token, operand1: Option[Any], operand2: Option[Any]): (Double, Double) = {
    (operand1, operand2) match {
      case (Some(d1: Double), Some(d2: Double)) => (d1, d2)
      case _ => throw RuntimeError(operator, "Operands must be numbers.")
    }
  }

  def stringify(evaluationRes: Option[Any]): String = evaluationRes match {
    case None => "nil"
    case Some(d: Double) =>
      val numberText: String = d.toString
      if(numberText.endsWith(".0")){
        numberText.substring(0, numberText.length - 2)
      }else{
        numberText
      }
    case Some(x) => x.toString
  }
}

case class RuntimeError(token: Token, message: String) extends RuntimeException(message)