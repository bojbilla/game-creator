package service

import akka.actor._
import database.MongoDatabaseService
import entities.Entities.{Possibility, Question, MultipleChoiceQuestion}
import mongodb.MongoDBEntities.{FBPageLike, FBPage}
import reactivemongo.api.{QueryOpts, DefaultDB}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.Count
import server.domain.RestMessage
import service.GameGenerator.{CreatedWhichPageDidYouLike, WhichPageDidYouLike}
import service.RandomDocumentGetter.{GetDocument, RetrievedDocument}

import scala.concurrent.{ExecutionContext, Promise, Future, ExecutionContextExecutor}
import scala.util.{Failure, Success, Random}

/**
 * Created by roger on 17/11/14.
 */

object GameGenerator {

  def props(database: DefaultDB): Props =
    Props(new GameGenerator(database))

  case class WhichPageDidYouLike(user_id: String) extends RestMessage
  case class CreatedWhichPageDidYouLike(mc: MultipleChoiceQuestion) extends RestMessage
}

class GameGenerator(database: DefaultDB) extends Actor with ActorLogging{
  implicit def dispatcher: ExecutionContextExecutor =  context.dispatcher

  implicit def actorRefFactory: ActorContext = context
  var unlikedPages: List[FBPage] = List()


  def receive = {
    case WhichPageDidYouLike(user_id) =>
      log.info("we do stuff")
      val client = sender()
      val pagesCollection = database[BSONCollection](MongoDatabaseService.fbPagesCollection)
      val likesCollection = database[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      val query = BSONDocument(
        "user_id" -> user_id
      )
      val likedPageIds = likesCollection.find(query).cursor[FBPageLike].collect[List]().map{
        likes =>
          likes.map {
          like =>
            like.page_id
        }
      }
      likedPageIds.map { pIds =>
        createUnlikedPages(database, pagesCollection, BSONDocument(), pIds).map{
          pages =>
            val likedPageId = Random.shuffle(pIds).head
            val likedPage = pagesCollection.find(BSONDocument{
              "page_id" -> likedPageId
            }).one[FBPage]
            likedPage.map{
              answerO => answerO.map { answer =>
                val question = Question("Which page did you like?", Some("Pick the right one"), None)
                val possibilities = (answer :: pages).map { c =>
                            val source = c.photos match {
                              case Some(p) => p.source
                              case None => None
                            }
                            Possibility(c.name, source)
                          }.toVector
                val answerPossibility = possibilities(0)
                val randomPossibilities = Random.shuffle(possibilities)
              val mc = MultipleChoiceQuestion("somerandomstring",
                user_id, question, randomPossibilities,
                randomPossibilities.indexOf(answerPossibility))
                log.info("we send stuff")
                client ! CreatedWhichPageDidYouLike(mc)
              }

            }
        }
      }

    case _ => log.error(s"GameGenerator received a unexpected message")
  }

  def createUnlikedPages(db: DefaultDB,
                         collection: BSONCollection,
                         query: BSONDocument,
                         liked: List[String]): Future[List[FBPage]] ={
    val promise = Promise[List[FBPage]]()
    def recurs(pages: List[FBPage]): Unit ={
      getDocument(db, collection, query).map{po => po.map { p =>
        if (pages.length >= 3){
          promise.complete(Success(pages))
        }
        if (!liked.contains(p.page_id)){
          recurs(p :: pages)
        } else {
          recurs(pages)
        }
      }}
    }
    recurs(List())
    promise.future
  }
  def getDocument(db: DefaultDB, collection: BSONCollection, query: BSONDocument): Future[Option[FBPage]] = {
  val futureCount = db.command(Count(collection.name, Some(query)))
    futureCount.flatMap { count =>
      val skip = Random.nextInt(count)
      collection.find(query).
        options(QueryOpts(skipN = skip)).one[FBPage]

    }
  }

}
