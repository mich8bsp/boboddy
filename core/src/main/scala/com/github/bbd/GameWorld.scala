package com.github.bbd

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.github.bbd.GameEntity.GameEntityId
import com.github.bbd.GameWorld.{ApplyAsyncEvents, ApplyAsyncStateTransitions, ApplyEntityEvents, GetReadOnlyEntities, RouteEvents, RunGameLogic}

import scala.collection.mutable
import scala.util.Success

class GameWorld(gameLogic: GameLogic) extends Actor{
  private val entities: mutable.Map[GameEntityId, GameEntity] = mutable.Map()

  override def receive: Receive = {
    case ApplyEntityEvents =>
      val newEvents: Seq[GameEvent] = entities.flatMap({
        case (_, entity) => entity.events.flatMap(ev => gameLogic.applyEventToEntity(entity, ev))
      }).toSeq

      entities.foreach(_._2.events.clear())

      sender().tell(newEvents, self)

    case RunGameLogic(dt) =>
      val logicRunResult: Map[GameEntityId, LogicOutput] = entities.values
        .map(entity => entity.id -> gameLogic.run(entity, dt))
        .toMap

      logicRunResult.foreach({
        case (entityId, logicOut) => logicOut.stateTransitions.foreach(transition => {
          transition.applyTo(entities(entityId))
        })
      })

      val logicRunEvents: Seq[GameEvent] = logicRunResult.values.flatMap(_.events).toSeq

      logicRunResult.foreach({
        case (entityId, logicOut) => {
          logicOut.asyncEvents.onComplete({
            case Success(v) if v.nonEmpty => self.tell(ApplyAsyncEvents(v), self)
            case _ =>
          })(context.dispatcher)

          logicOut.asyncStateTransitions.onComplete({
            case Success(v) if v.nonEmpty => self.tell(ApplyAsyncStateTransitions(entityId, v), self)
            case _ =>
          })(context.dispatcher)
        }
      })

      sender().tell(logicRunEvents, self)

    case ApplyAsyncEvents(events) =>
      self.tell(RouteEvents(events.flatMap({
        case x: ToEntityEvent => Some(x)
        case _ => None
      })), self)

    case ApplyAsyncStateTransitions(entityId, transitions) =>
      transitions.foreach(transition => transition.applyTo(entities(entityId)))

    case RouteEvents(events) =>
      events.foreach(ev => entities(ev.toId).events.append(ev))

    case GetReadOnlyEntities =>
      val readOnlyEntities: Seq[ReadOnlyEntity] = entities.values.map(_.toReadOnlyEntity).toSeq
      sender().tell(readOnlyEntities, self)
  }
}

object GameWorld{
  private val actorSystem = ActorSystem("game-world")
  def create(gameLogic: GameLogic): ActorRef = actorSystem.actorOf(Props(new GameWorld(gameLogic)))

  case object ApplyEntityEvents
  case class ApplyAsyncEvents(events: Seq[GameEvent])
  case class ApplyAsyncStateTransitions(entityId: GameEntityId, transitions: Seq[ConditionalEntityStateTransition])
  case class RunGameLogic(dt: Float)
  case class RouteEvents(events: Seq[ToEntityEvent])
  case object GetReadOnlyEntities
}
