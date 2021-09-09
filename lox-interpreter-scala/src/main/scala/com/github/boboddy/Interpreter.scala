package com.github.boboddy

class Interpreter {
  private val env: Environment = new Environment()

  def interpret(statements: Seq[Stmt]): Unit = {
    try {
      statements.foreach({
        case PrintStmt(expr) => println(stringify(evaluate(expr)))
        case ExpressionStmt(expr) => evaluate(expr)
        case VarStmt(name, initializer) =>
          val value: Option[Any] = initializer.flatMap(evaluate)
          env.define(name.lexeme, value)
      })
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
          if(right == 0D){
            throw RuntimeError(operator, "/ by zero")
          }else{
            Some(left / right)
          }
        case TokenType.STAR =>
          val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
          Some(left * right)
        case TokenType.PLUS =>
          (leftValue, rightValue) match {
            case (Some(d1: Double), Some(d2: Double)) => Some(d1 + d2)
            case (Some(s1: String), Some(s2: String)) => Some(s1 + s2)
            case (Some(s1: String), Some(x)) => Some(s1 + x.toString)
            case (Some(x), Some(s1: String)) => Some(x.toString + s1)
            case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
          }
        case TokenType.LESS =>
          (leftValue, rightValue) match {
            case (Some(d1: Double), Some(d2: Double)) => Some(d1 < d2)
            case (Some(s1: String), Some(s2: String)) => Some(s1 < s2)
            case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
          }
        case TokenType.LESS_EQUAL =>
          (leftValue, rightValue) match {
            case (Some(d1: Double), Some(d2: Double)) => Some(d1 <= d2)
            case (Some(s1: String), Some(s2: String)) => Some(s1 <= s2)
            case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
          }
        case TokenType.GREATER =>
          (leftValue, rightValue) match {
            case (Some(d1: Double), Some(d2: Double)) => Some(d1 > d2)
            case (Some(s1: String), Some(s2: String)) => Some(s1 > s2)
            case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
          }
        case TokenType.GREATER_EQUAL =>
          (leftValue, rightValue) match {
            case (Some(d1: Double), Some(d2: Double)) => Some(d1 >= d2)
            case (Some(s1: String), Some(s2: String)) => Some(s1 >= s2)
            case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
          }
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
    case VariableExpr(name) => env.get(name)
    case AssignExpr(name, value) => {
      val evaluatedValue = evaluate(value)
      env.assign(name, evaluatedValue)
      evaluatedValue
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