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

      val result = likedPageIds.flatMap{ pIds =>
        val queryUnliked = BSONDocument(
          "page_id" -> BSONDocument("$nin" -> pIds)
        )
        pagesCollection.find(queryUnliked).cursor[FBPage].collect[List](3).flatMap{ unlikedPages =>
          if (unlikedPages.length < 3){
            Future.failed(new Exception("Not enough unliked pages"))
          } else {
            val likedPageId = Random.shuffle(pIds).head
            val likedPage = pagesCollection.find(BSONDocument {
              "page_id" -> likedPageId
            }).one[FBPage]
            likedPage.map{ _.map { answer =>
              val question = Question("WhichPageDidYouLike")

              val possibilities = (answer :: unlikedPages).map { c =>
                val source = c.photos match {
                  case Some(p) => p.source
                  case None => Some("")
                }
                Possibility(c.name, source, Some(c.page_id))
              }.toVector

              val answerPossibility = possibilities(0)
              val randomPossibilities = Random.shuffle(possibilities)

              MultipleChoiceQuestion(answerPossibility.text.get,
                user_id, question, randomPossibilities,
                randomPossibilities.indexOf(answerPossibility))
            }
          }

          }
        }

      }

      result.onComplete{
        case Success(Some(question)) => client ! FinishedQuestionCreation(question)
        case Failure(e) =>
          log.error("Failed to created WhichPageDidYouLike as this user likes EVERYTHING " + e)
          client ! FailedToCreateQuestion("Unable to create question WhichPageDidYouLike", MCWhichPageDidYouLike)
        case _ =>
          log.error("Failed to created WhichPageDidYouLike as this user likes EVERYTHING ")
          client ! FailedToCreateQuestion("Unable to create question WhichPageDidYouLike", MCWhichPageDidYouLike)

      }

    case x => log.error(s"WhichPageDidYouLike received a unexpected message " + x)
  }

}
