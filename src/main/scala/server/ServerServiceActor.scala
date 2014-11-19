package server

import akka.actor.{ActorRef, ActorLogging}
import data_gathering.DataRetriever
import mock.MockServiceActor
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}
import service.GameCreatorServiceActor
import org.json4s.{DefaultFormats, Formats}
import reactivemongo.api._
import scala.concurrent.ExecutionContext.Implicits.global
import mongodb.MongoDBEntities._


/**
 * Created by roger on 10/11/14.
 */
class ServerServiceActor extends MockServiceActor with GameCreatorServiceActor with ActorLogging {
  override def actorRefFactory = context
  override def receive = runRoute(mockRoutes ~ gameCreatorRoutes)
  val driver = new MongoDriver
  val connection: MongoConnection = driver.connection(List("localhost"))
  override val db: DefaultDB = connection("mydb")

  override implicit def json4sFormats: Formats = DefaultFormats

  override val crawlerHost: String = "http://localhost:9000"
  override val dataRetriever: ActorRef = context.actorOf(DataRetriever.props())

}
