package com.github.bbd

import com.github.bbd.GameEntity.GameEntityId

import scala.collection.mutable

trait GameEntity {
  val id: GameEntityId
  val events: mutable.Queue[ToEntityEvent] = mutable.Queue[ToEntityEvent]()
  def toReadOnlyEntity: ReadOnlyEntity
}

trait ReadOnlyEntity

object GameEntity{
  type GameEntityId = String
}

trait EntityStateTransition{
  def applyTo(entity: GameEntity): Unit
}

trait ConditionalEntityStateTransition extends EntityStateTransition {
  def canBeApplied(entity: GameEntity): Boolean

  override def applyTo(entity: GameEntity): Unit = {
    if(canBeApplied(entity)){
     super.applyTo(entity)
    }else{
      ()
    }
  }
}