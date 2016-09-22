package me.reminisce.gameboard.questions

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.board.GameboardEntities.TimelineQuestion
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import me.reminisce.gameboard.questions.TimeQuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Factory for [[me.reminisce.gameboard.questions.WhenDidYouLikeThisPage]]
  */
object WhenDidYouLikeThisPage {

  /**
    * Creates a WhenDidYouLikeThisPage question generator
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
    Props(new WhenDidYouLikeThisPage(database))
}

/**
  * WhenDidYouLikeThisPage question generator
  * @param db database from which to take the data
  */
class WhenDidYouLikeThisPage(db: DefaultDB) extends TimeQuestionGenerator {

  /**
    * Entry point for this actor, handles the CreateQuestionWithMultipleItems(userId, itemIds) message by getting the
    * necessary items from the database and creating a question. If some items are non conform to what is expected,
    * missing or there is an error while contacting the database, the error is reported to the client.
    * @return Nothing
    */
  def receive = {
    case CreateQuestion(userId, itemId) =>
      val client = sender()
      val query = BSONDocument(
        "userId" -> userId,
        "pageId" -> itemId
      )
      val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)

      (for {
        mayBePageLike <- pageLikesCollection.find(query).one[FBPageLike]
        pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
        pageSelector = BSONDocument("pageId" -> itemId)
        maybePage <- pagesCollection.find(pageSelector).one[FBPage]
      }
        yield {
          val tlQuestionOpt =
            for {
              pageLike <- mayBePageLike
              page <- maybePage
            }
              yield {
                val actualDate = pageLike.likeTime
                generateRange(actualDate) match {
                  case (min, default, max, unit, step) =>
                    val threshold = 0
                    val pageSubject = QuestionGenerator.subjectFromPage(maybePage.get)
                    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
                    TimelineQuestion(userId, Timeline, TLWhenDidYouLikeThisPage, Some(pageSubject),
                      actualDate.toString(formatter), min.toString(formatter), max.toString(formatter),
                      default.toString(formatter), unit, step, threshold)
                }
              }
          tlQuestionOpt match {
            case Some(tlQuestion) =>
              client ! FinishedQuestionCreation(tlQuestion)
            case None =>
              client ! NotEnoughData(s"Page or pagelike not found : user $userId, page $itemId")
          }
        }) onFailure {
        case e =>
          client ! MongoDBError(s"${e.getMessage}")
      }

    case any =>
      log.error(s"Unknown message : $any.")
  }

}
