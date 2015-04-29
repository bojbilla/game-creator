package me.reminisce.server

import akka.actor.{Actor, ActorLogging}
import me.reminisce.service.gameboardgen.GameCreatorServiceActor
import org.json4s.{DefaultFormats, Formats}
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Properties._


class ServerServiceActor extends Actor with GameCreatorServiceActor with ActorLogging {
  override def actorRefFactory = context

  override def receive = runRoute(gameCreatorRoutes)

  val driver = new MongoDriver
  val mongoHost = envOrElse("MONGODB_HOST", Server.hostName)
  val mongodbName = envOrElse("REMINISCE_MONGO_DB", "mydb")
  val connection: MongoConnection = driver.connection(List(mongoHost))
  override val db: DefaultDB = connection(mongodbName)

  override implicit def json4sFormats: Formats = DefaultFormats

}
