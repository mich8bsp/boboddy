package com.github.bbd

import com.github.bbd.GameEntity.GameEntityId

import scala.collection.mutable

trait GameEntity {
  def applyTransition(transition: EntityStateTransition): Unit

  val id: GameEntityId
  val events: mutable.Queue[ToEntityEvent] = mutable.Queue[ToEntityEvent]()
}

object GameEntity{
  type GameEntityId = String
}

trait EntityStateTransition{
  def applyTo(entity: GameEntity): Unit
}