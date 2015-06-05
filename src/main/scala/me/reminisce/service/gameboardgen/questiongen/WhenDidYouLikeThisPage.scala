package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{TimeUnit, TimelineQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Random, Success}


object WhenDidYouLikeThisPage {

  def props(database: DefaultDB): Props =
    Props(new WhenDidYouLikeThisPage(database))
}

class WhenDidYouLikeThisPage(db: DefaultDB) extends QuestionGenerator {
  def receive = {
    case CreateQuestion(userId, itemId) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      val client = sender()
      val query = BSONDocument(
        "userId" -> userId,
        "pageId" -> itemId
      )
      val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      pageLikesCollection.find(query).one[FBPageLike].onComplete {
        case Success(maybePageLike) =>
          val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
          val pageSelector = BSONDocument("pageId" -> itemId)

          pagesCollection.find(pageSelector).one[FBPage].onComplete {
            case Success(maybePage) =>
              val pageLike = maybePageLike.get
              val actualDate = pageLike.likeTime
              val unit = TimeUnit.Day
              val step = 1
              val threshold = 3
              val rangeSize = 30
              val rangeBefore = Random.nextInt(rangeSize)
              val min = actualDate - rangeBefore.days
              val max = actualDate + (rangeSize - rangeBefore).days
              val pageSubject = subjectFromPage(maybePage.get)
              val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
              val tlQuestion = TimelineQuestion(userId, Timeline, TLWhenDidYouLikeThisPage, Some(pageSubject),
                actualDate.toString(formatter), min.toString(formatter), max.toString(formatter), min.toString(formatter),
                unit, step, threshold)
              client ! FinishedQuestionCreation(tlQuestion)

            case Failure(e) =>
              client ! MongoDBError(s"${e.getMessage}")
          }

        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
      }
  }

}
