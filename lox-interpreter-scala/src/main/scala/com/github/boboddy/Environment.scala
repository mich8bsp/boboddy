package com.github.boboddy

import scala.collection.mutable

class Environment {
  private val values: mutable.Map[String, Option[Any]] = mutable.Map[String, Option[Any]]()

  def define(name: String, value: Option[Any]): Unit = {
    values.put(name, value)
  }

  def get(name: Token): Option[Any] = {
    values.getOrElse(name.lexeme, throw RuntimeError(name, s"Undefined variable '${name.lexeme}'."))
  }

  def assign(name: Token, value: Option[Any]): Unit = {
    if(values.contains(name.lexeme)){
      values.put(name.lexeme, value)
    }else{
      throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
    }
  }
}
