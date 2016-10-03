package me.reminisce.database

import akka.actor.{Actor, ActorContext, ActorLogging}
import akka.event.Logging

/**
  * Defines the basic content of a Database service
  * Mixed in [[me.reminisce.database.MongoDatabaseService]]
  */
trait DatabaseService extends Actor with ActorLogging {
  /**
    * Gives a reference to the actor context
    * @return the current actor context
    */
  def actorRefFactory: ActorContext = context

}
