package service

import akka.actor.{Props, Actor, ActorLogging}
import mongodb.MongoDBEntities.FBPage
import reactivemongo.api.{QueryOpts, DefaultDB}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.Count
import service.RandomDocumentGetter.{RetrievedDocument, GetDocument}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
 * Created by roger on 18/11/14.
 */

object RandomDocumentGetter{
  case class GetDocument()
  case class RetrievedDocument[T](entity: T)

  def props[T](db: DefaultDB, collection: BSONCollection, query: BSONDocument): Props =
    Props(new RandomDocumentGetter[T](db, collection, query))

}
class RandomDocumentGetter[T](db: DefaultDB, collection: BSONCollection, query: BSONDocument) extends Actor with ActorLogging{
  def receive = {
    case GetDocument =>
//      val client = sender()
//      val futureCount = db.command(Count(collection.name, Some(query)))
//      val result = futureCount.flatMap { count =>
//        log.info("let me count " + count)
//        val skip = Random.nextInt(count)
//        collection.find(query).
//          options(QueryOpts(skipN = skip)).one[T]
//      }
//      result.map {
//        case Some(entity) => client ! RetrievedDocument(entity)
//        case _ => self ! GetDocument
//      }
//
  }

}
