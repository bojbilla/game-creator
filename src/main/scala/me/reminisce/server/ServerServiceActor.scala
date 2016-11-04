package me.reminisce.server

import akka.actor.{Actor, ActorLogging}
import org.json4s.{DefaultFormats, Formats}
import reactivemongo.api.{MongoConnection, MongoDriver}

/**
  * The actor handling the http request in this application. It handles the requests according to the
  * routes defined in [[me.reminisce.server.GameCreatorService]]
  *
  * @param mongoHost   host for the mongodb
  * @param mongoDbName name of the mongoDb to connect to
  */
class ServerServiceActor(mongoHost: String, mongoDbName: String) extends Actor with GameCreatorServiceActor with ActorLogging {
  override def actorRefFactory = context

  override def receive = runRoute(gameCreatorRoutes)

  val driver = new MongoDriver
  override val dbName = mongoDbName
  override val dbConnection: MongoConnection = driver.connection(List(mongoHost))

  override implicit def json4sFormats: Formats = DefaultFormats

  /**
    * Cascades the shutdown to the mongo driver.
    */
  override def postStop(): Unit = {
    dbConnection.close()
    driver.system.terminate()
    driver.close()
  }

}
