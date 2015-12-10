package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPage
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities._
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}


object WhichPageDidYouLike {
  def props(database: DefaultDB): Props =
    Props(new WhichPageDidYouLike(database))

}


class WhichPageDidYouLike(db: DefaultDB) extends QuestionGenerator {

  def receive = {
    case CreateQuestion(userId, itemId) =>
      val client = sender()
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
      val likesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      fetchPage(pagesCollection, itemId, client) {
        case Some(page) =>
          fetchLikedPages(likesCollection, userId, client) {
            list =>
              val ids = list.map(fbPageLike => fbPageLike.pageId)
              val queryNotLiked = BSONDocument(
                "pageId" -> BSONDocument("$nin" -> ids)
              )
              getDocuments[FBPage](db, pagesCollection, queryNotLiked, 3).onComplete {
                case Success(listPages) =>
                  if (listPages.length < 3) {
                    client ! NotEnoughData(s"Unable to create question : not enough not liked pages.")
                  } else {
                    val possibilities = (page :: listPages).map {
                      pge =>
                        val url = pge.photos match {
                          case Some(p) => p.source
                          case None => None
                        }
                        Possibility(pge.name.get, url, "Page", Some(pge.pageId))
                    }
                    possibilities.headOption match {
                      case Some(answer) =>
                        val shuffled = Random.shuffle(possibilities)
                        val gameQuestion = MultipleChoiceQuestion(userId, MultipleChoice, MCWhichPageDidYouLike, None, shuffled, shuffled.indexOf(answer))
                        client ! FinishedQuestionCreation(gameQuestion)
                      case None =>
                        client ! NotEnoughData(s"This should not happen, but the possibilities were empty.")
                    }
                  }
                case Failure(e) =>
                  client ! MongoDBError(s"${e.getMessage}")
                case any =>
                  client ! MongoDBError(s"Unknown error : $any.")
              }
          }
        case None =>
          client ! NotEnoughData(s"Page not found. $itemId")
      }
    case any =>
      log.error(s"WhichPageDidYouLike received a unexpected message $any")
  }

}
