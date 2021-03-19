package com.github.bbd

trait GameLogic {
  def run(world: GameWorld, entity: GameEntity, dt: Float): LogicOutput
}

case class LogicOutput(
                      events: Seq[GameEvent],
                      stateTransitions: Seq[EntityStateTransition]
                      )