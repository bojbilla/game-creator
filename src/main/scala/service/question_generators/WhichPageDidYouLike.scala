package service.question_generators

import akka.actor.{ActorLogging, Actor, Props}
import database.MongoDatabaseService
import entities.Entities.{Possibility, Question, MultipleChoiceQuestion}
import mongodb.MongoDBEntities.{FBPage, FBPageLike}
import reactivemongo.api.{QueryOpts, DefaultDB}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.Count
import service.question_generators.QuestionGenerator.{FailedToCreateQuestion, CreateQuestion, FinishedQuestionCreation}


import scala.concurrent.{Promise, Future}
import scala.util.{Failure, Success, Random}
import entities.Entities.SpecificQuestionType._


/**
 * Created by roger on 23/12/14.
 */

object WhichPageDidYouLike {
  def props(database: DefaultDB): Props =
    Props(new WhichPageDidYouLike(database))

}


class WhichPageDidYouLike(db: DefaultDB) extends QuestionGenerator{

  def receive = {
    case CreateQuestion(user_id) =>
      val client = sender()
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
      val likesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      val query = BSONDocument(
        "user_id" -> user_id
      )
      val likedPageIds = likesCollection.find(query).cursor[FBPageLike].collect[List]().map {
        likes =>
          likes.map {
            like =>
              like.page_id
          }
      }
      likedPageIds.onComplete {
        case Success(pIds) =>
        createUnlikedPages(db, pagesCollection, BSONDocument(), pIds)onComplete {
          case Success(pages) =>
            val likedPageId = Random.shuffle(pIds).head
            val likedPage = pagesCollection.find(BSONDocument {
              "page_id" -> likedPageId
            }).one[FBPage]
            likedPage.onComplete {
              case Success(Some(answer)) =>
                val question = Question("WhichPageDidYouLike")
                val possibilities = (answer :: pages).map { c =>
                  val source = c.photos match {
                    case Some(p) => p.source
                    case None => Some("")
                  }
                  Possibility(c.name, source, Some(c.page_id))
                }.toVector
                val answerPossibility = possibilities(0)
                val randomPossibilities = Random.shuffle(possibilities)
                val mc = MultipleChoiceQuestion(answerPossibility.text.get,
                  user_id, question, randomPossibilities,
                  randomPossibilities.indexOf(answerPossibility))
                client ! FinishedQuestionCreation(mc)
              case x =>
                client ! FailedToCreateQuestion("WhichPagedidYouLike failed", MCWhichPageDidYouLike)
              }
          case Failure(t) =>
            client ! FailedToCreateQuestion("WhichPagedidYouLike failed " + t.getMessage, MCWhichPageDidYouLike)
        }

        case Failure(t) =>
          client ! FailedToCreateQuestion("WhichPagedidYouLike failed " + t.getMessage, MCWhichPageDidYouLike)
      }

    case x => log.error(s"WhichPageDidYouLike received a unexpected message " + x)
  }

  def createUnlikedPages(db: DefaultDB,
                         collection: BSONCollection,
                         query: BSONDocument,
                         liked: List[String]): Future[List[FBPage]] ={
    val promise = Promise[List[FBPage]]()
    def recurs(pages: List[FBPage], forbiddenPageIds: List[String]): Unit ={
      getDocument(db, collection, query).map{po => po.map { p =>
        if (pages.length >= 3){
          promise.complete(Success(pages))
        }
        if (!forbiddenPageIds.contains(p.page_id)){
          recurs(p :: pages, p.page_id :: forbiddenPageIds)
        } else {
          recurs(pages, forbiddenPageIds)
        }
      }}
    }
    recurs(List(), liked)
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
