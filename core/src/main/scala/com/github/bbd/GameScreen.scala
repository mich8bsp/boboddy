package com.github.bbd

import akka.actor.ActorRef
import com.badlogic.gdx.Screen
import akka.pattern.ask
import akka.util.Timeout

import java.util.concurrent.TimeUnit
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

trait GameScreen extends Screen{

  val world: ActorRef
  private implicit val timeout: Timeout = Timeout(Duration(1, TimeUnit.SECONDS))
  implicit val ec: ExecutionContext

  override def show(): Unit = ???

  override def render(delta: Float): Unit = {
    update(delta)
    val entitiesToRender: Seq[ReadOnlyEntity] = Await.result(world.ask(GameWorld.GetReadOnlyEntities)
      .mapTo[Seq[ReadOnlyEntity]], timeout.duration)

    entitiesToRender.foreach(entity => renderEntity(entity, delta))
  }

  def update(delta: Float): Unit = {
    val eventsFromPrevFrameFuture: Future[Seq[GameEvent]] = world.ask(GameWorld.ApplyEntityEvents)
      .mapTo[Seq[GameEvent]]
    val eventsFromGameLogicFuture: Future[Seq[GameEvent]] = world.ask(GameWorld.RunGameLogic(delta))
      .mapTo[Seq[GameEvent]]

    val allEvents: Seq[GameEvent] = Await.result(for{
      eventsFromPrevFrame <- eventsFromPrevFrameFuture
      eventsFromGameLogic <- eventsFromGameLogicFuture
    }yield {
      eventsFromPrevFrame ++ eventsFromGameLogic
    }, timeout.duration)

    //route events from this frame to the relevant entity queues
    val eventsToRoute: Seq[ToEntityEvent] = allEvents.flatMap({
      case event: ToEntityEvent => Some(event)
      case _ => None
    })

    world.tell(GameWorld.RouteEvents(eventsToRoute), ActorRef.noSender)
  }

  def renderEntity(entity: ReadOnlyEntity, fl: Float): Unit

  override def resize(width: Int, height: Int): Unit = ???

  override def pause(): Unit = ???

  override def resume(): Unit = ???

  override def hide(): Unit = ???

  override def dispose(): Unit = ???
}
