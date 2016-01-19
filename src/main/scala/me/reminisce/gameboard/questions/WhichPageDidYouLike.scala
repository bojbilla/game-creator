package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoDBEntities.FBPage
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.board.GameboardEntities._
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}

/**
  * Factory for [[me.reminisce.gameboard.questions.WhichPageDidYouLike]]
  */
object WhichPageDidYouLike {

  /**
    * Creates a WhichPageDidYouLike question generator
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
    Props(new WhichPageDidYouLike(database))
}

/**
  * WhichPageDidYouLike question generator
  * @param db database from which to take the data
  */
class WhichPageDidYouLike(db: DefaultDB) extends QuestionGenerator {

  /**
    * Entry point for this actor, handles the CreateQuestionWithMultipleItems(userId, itemIds) message by getting the
    * necessary items from the database and creating a question. If some items are non conform to what is expected,
    * missing or there is an error while contacting the database, the error is reported to the client.
    * @return Nothing
    */
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
