package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.TimelineQuestion
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import me.reminisce.service.gameboardgen.questiongen.TimeQuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global


object WhenDidYouLikeThisPage {

  def props(database: DefaultDB): Props =
    Props(new WhenDidYouLikeThisPage(database))
}

class WhenDidYouLikeThisPage(db: DefaultDB) extends TimeQuestionGenerator {
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
                  case (min, max, unit) =>
                    val step = 1
                    val threshold = 0
                    val pageSubject = QuestionGenerator.subjectFromPage(maybePage.get)
                    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
                    TimelineQuestion(userId, Timeline, TLWhenDidYouLikeThisPage, Some(pageSubject),
                      actualDate.toString(formatter), min.toString(formatter), max.toString(formatter),
                      min.toString(formatter), unit, step, threshold)
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
      log.error(s"Uknown message : $any.")
  }

}
