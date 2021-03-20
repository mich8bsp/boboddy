package com.github.bbd

import scala.concurrent.Future

trait GameLogic {
  def run(entity: GameEntity, dt: Float): LogicOutput
  def applyEventToEntity(entity: GameEntity, event: ToEntityEvent): Seq[GameEvent]
}

case class LogicOutput(
                      events: Seq[GameEvent] = Seq.empty[GameEvent],
                      asyncEvents: Future[Seq[GameEvent]] = Future.successful(Seq.empty[GameEvent]),
                      stateTransitions: Seq[EntityStateTransition] = Seq.empty[EntityStateTransition],
                      //async transitions have to be conditional because by the time they're applied, they might be illegal or irrelevant
                      asyncStateTransitions: Future[Seq[ConditionalEntityStateTransition]] = Future.successful(Seq.empty[ConditionalEntityStateTransition])
                      )