package me.reminisce.database

import akka.actor.{Actor, ActorContext, ActorLogging}
import akka.event.Logging

trait DatabaseService extends Actor with ActorLogging {
  def actorRefFactory: ActorContext = context

  override val log = Logging(context.system, this)

}
