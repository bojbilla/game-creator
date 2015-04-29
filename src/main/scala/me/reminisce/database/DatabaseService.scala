package me.reminisce.database

import akka.actor.{Actor, ActorContext, ActorLogging}

trait DatabaseService extends Actor with ActorLogging {
  def actorRefFactory: ActorContext = context

}
