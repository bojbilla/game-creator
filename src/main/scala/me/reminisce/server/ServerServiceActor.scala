package me.reminisce.server

import akka.actor.{ActorLogging, ActorRef}
import me.reminisce.fetcher.FetcherService
import me.reminisce.mock.MockServiceActor
import me.reminisce.service.GameCreatorServiceActor
import org.json4s.{DefaultFormats, Formats}
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Properties._


/**
 * Created by roger on 10/11/14.
 */
class ServerServiceActor extends MockServiceActor with GameCreatorServiceActor with ActorLogging {
  override def actorRefFactory = context

  override def receive = runRoute(mockRoutes ~ gameCreatorRoutes)

  val driver = new MongoDriver
  val mongoHost = envOrElse("MONGODB_HOST", Server.hostName)
  val mongodbName = envOrElse("REMINISCE_MONGO_DB", "mydb")
  val connection: MongoConnection = driver.connection(List(mongoHost))
  override val db: DefaultDB = connection(mongodbName)

  override implicit def json4sFormats: Formats = DefaultFormats

  override val dataRetriever: ActorRef = context.actorOf(FetcherService.props(db))

}
