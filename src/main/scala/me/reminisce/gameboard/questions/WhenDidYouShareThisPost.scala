package me.reminisce.gameboard.questions

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDBEntities.FBPost
import me.reminisce.database.{MongoDBEntities, MongoDatabaseService}
import me.reminisce.gameboard.board.GameboardEntities
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.board.GameboardEntities.TimelineQuestion
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import me.reminisce.gameboard.questions.TimeQuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


object WhenDidYouShareThisPost {

  def props(database: DefaultDB): Props =
    Props(new WhenDidYouShareThisPost(database))
}

class WhenDidYouShareThisPost(db: DefaultDB) extends TimeQuestionGenerator {
  def receive = {
    case CreateQuestion(userId, itemId) =>
      val client = sender()
      val query = BSONDocument(
        "userId" -> userId,
        "postId" -> itemId
      )
      val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      postCollection.find(query).one[FBPost].onComplete {
        case Success(postOpt) =>
          postOpt match {
            case Some(post) =>
              val dateString = post.createdTime.getOrElse("1970-01-01'T'00:00:00+0000")
              val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
              val actualDate = formatter.parseDateTime(dateString)
              val threshold = 0
              val postSubject = QuestionGenerator.subjectFromPost(post)
              generateRange(actualDate) match {
                case (min, default, max, unit, step) =>
                  val tlQuestion = TimelineQuestion(userId, Timeline, TLWhenDidYouShareThisPost, Some(postSubject),
                    actualDate.toString(formatter), min.toString(formatter), max.toString(formatter),
                    default.toString(formatter), unit, step, threshold)
                  client ! FinishedQuestionCreation(tlQuestion)
              }
            case None =>
              client ! NotEnoughData(s"Post not found : $itemId")
          }
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
        case any =>
          client ! MongoDBError(s"Unknown error: $any.")
      }
    case any =>
      log.error(s"OrderByPageLikes received unknown message: $any.")
  }

}
