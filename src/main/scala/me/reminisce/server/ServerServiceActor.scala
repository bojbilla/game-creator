package me.reminisce.server

import akka.actor.{Actor, ActorLogging}
import org.json4s.{DefaultFormats, Formats}
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}

import scala.concurrent.ExecutionContext.Implicits.global


class ServerServiceActor extends Actor with GameCreatorServiceActor with ActorLogging {
  override def actorRefFactory = context

  override def receive = runRoute(gameCreatorRoutes)

  val driver = new MongoDriver
  val mongoHost = ApplicationConfiguration.mongoHost
  val mongodbName = ApplicationConfiguration.mongodbName
  val connection: MongoConnection = driver.connection(List(mongoHost))
  override val db: DefaultDB = connection(mongodbName)

  override implicit def json4sFormats: Formats = DefaultFormats

  override def postStop(): Unit = {
    connection.close()
    driver.system.shutdown()
    driver.close()
  }

}
