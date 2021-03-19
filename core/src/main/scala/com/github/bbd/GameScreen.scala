package com.github.bbd

import com.badlogic.gdx.Screen

trait GameScreen extends Screen{

  val world: GameWorld
  val gameLogic: GameLogic

  override def show(): Unit = ???

  override def render(delta: Float): Unit = {
    update(world, delta)
    render(world, delta)
  }

  def update(world: GameWorld, delta: Float): Unit = {
    val eventsFromUpdate: Seq[GameEvent] = world.entities.toSeq.flatMap{
      case (_, entity) => {
        val replyEvents: Seq[GameEvent] = entity.events.flatMap(ev => {
          //apply all events from previous frame to entities and maybe generate new events
          applyEventToEntity(entity, ev)
        }).toSeq

        val gameLogicOut: LogicOutput = gameLogic.run(world, entity, delta)

        //some effects can be applied directly without generating an event
        gameLogicOut.stateTransitions.foreach(transition => entity.applyTransition(transition))

        replyEvents ++ gameLogicOut.events
      }
    }

    //route events from this frame to the relevant entity queues
    eventsFromUpdate.foreach({
      case event: ToEntityEvent => world.entities(event.toId).events.append(event)
      case event => ???
    })
  }

  def applyEventToEntity(entity: GameEntity, event: ToEntityEvent): Seq[GameEvent]

  def render(world: GameWorld, dt: Float): Unit = {
    world.entities.foreach({
      case (id, entity) => renderEntity(entity, dt)
    })
    //render world entities
  }

  def renderEntity(entity: GameEntity, fl: Float): Unit

  override def resize(width: Int, height: Int): Unit = ???

  override def pause(): Unit = ???

  override def resume(): Unit = ???

  override def hide(): Unit = ???

  override def dispose(): Unit = ???
}
