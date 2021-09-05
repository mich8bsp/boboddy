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
    } finally {
      source.close()
    }
  }

  def runPrompt(): Unit = {
    var line = StdIn.readLine()
    while(line != null){
      run(line)
      ErrorHandler.hadError = false
      line = StdIn.readLine()
    }
  }

  private def run(source: String): Unit = {
    val scanner: Scanner = new Scanner(source)
    val tokens: Seq[Token] = scanner.scanTokens()
    tokens.foreach(println)
  }



  def main(args: Array[String]): Unit = args match {
    case Array() => runPrompt()
    case Array(script) => runFile(script)
    case _ => println("Usage: scalox [script]")
      System.exit(64)
  }
}
