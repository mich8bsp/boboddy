package com.github.bbd

import com.github.bbd.GameEntity.GameEntityId

trait GameEvent

trait FromEntityEvent extends GameEvent{
  val fromId: GameEntityId
}

trait ToEntityEvent extends GameEvent {
  val toId: GameEntityId
}