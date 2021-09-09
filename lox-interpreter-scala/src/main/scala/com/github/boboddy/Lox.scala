package com.github.boboddy

import scala.io.{Source, StdIn}

object Lox {

  def runFile(path: String): Unit = {
    val source = Source.fromFile(path)
    try {
      val fileContent: String = source.getLines().mkString("\n")
      run(fileContent)

      if(ErrorHandler.hadError){
        System.exit(65)
      }
      if(ErrorHandler.hadRuntimeError){
        System.exit(70)
      }
    } finally {
      source.close()
    }
  }

  def runPrompt(): Unit = {
    var line = StdIn.readLine()
    while(line != null){
      run(line)
      ErrorHandler.hadError = false
      ErrorHandler.hadRuntimeError = false
      line = StdIn.readLine()
    }
  }

  private def run(source: String): Unit = {
    val scanner: Scanner = new Scanner(source)
    val tokens: Seq[Token] = scanner.scanTokens()
    val parser: Parser = new Parser(tokens)
    val parsedAst: Seq[Stmt] = parser.parse()
    parsedAst match {
      case Nil =>
      case _ if ErrorHandler.hadError =>
      case ast => Interpreter.interpret(ast)
    }
  }



  def main(args: Array[String]): Unit = args match {
    case Array() => runPrompt()
    case Array(script) => runFile(script)
    case _ => println("Usage: scalox [script]")
      System.exit(64)
  }
}
