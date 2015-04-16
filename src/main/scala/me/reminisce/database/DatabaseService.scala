package me.reminisce.database

import akka.actor.{Actor, ActorContext, ActorLogging}

/**
 * Created by roger on 17/11/14.
 */
trait DatabaseService extends Actor with ActorLogging {
  def actorRefFactory: ActorContext = context

}
