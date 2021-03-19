package com.github.bbd

import com.github.bbd.GameEntity.GameEntityId

import scala.collection.mutable

class GameWorld {
  val entities: mutable.Map[GameEntityId, GameEntity] = mutable.Map()
}
