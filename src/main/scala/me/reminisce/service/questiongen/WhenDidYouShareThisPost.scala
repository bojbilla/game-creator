package me.reminisce.service.questiongen

import akka.actor.Props
import me.reminisce.entities.Entities
import me.reminisce.entities.Entities.SpecificQuestionType._
import me.reminisce.entities.Entities.{Question, TimelineQuestion}
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.questiongen.QuestionGenerator.{FailedToCreateQuestion, FinishedQuestionCreation, CreateQuestion}
import org.joda.time.format.DateTimeFormat
import reactivemongo.api.DefaultDB
import reactivemongo.bson.{BSONArray, BSONDocument}

import scala.util.{Failure, Success, Random}

/**
 * Created by roger on 20/11/14.
 */

object WhenDidYouShareThisPost {

  def props(database: DefaultDB): Props =
    Props(new WhenDidYouShareThisPost(database))
}

class WhenDidYouShareThisPost(db: DefaultDB) extends PostQuestionGenerator(db) {
  def receive = {
    case CreateQuestion(user_id) =>
      val client = sender()
      val query = BSONDocument(
        "user_id" -> user_id,
        "message" -> BSONDocument("$exists" -> "true"),
        "type" -> BSONDocument("$nin" -> BSONArray("photo", "video"))
      )
      getDocument(db, collection, query).onComplete {
        case Success(optPost) => optPost match {
          case Some(post: FBPost) =>
            val bareQuestion = Question("WhenDidYouShareThisPost", Some(List(post.message.getOrElse(""))))
            val index = post.created_time.get.indexOf('T')

            val dateString = post.created_time.get.substring(0, index)
            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
            val range = 60
            val lower = Random.nextInt(range)
            val upper = range - lower
            val actualDate = formatter.parseDateTime(dateString)
            val minDate = lower
            val maxDate = upper
            val timeQuestion = TimelineQuestion(post.post_id, user_id, bareQuestion, minDate, maxDate, 10, actualDate)
            client ! FinishedQuestionCreation(timeQuestion)
          case None => client ! FailedToCreateQuestion(s"Something went wrong WhenDidYouShareThisPost user: $user_id", TLWhenDidYouShareThisPost)
        }
        case Failure(e) => client ! FailedToCreateQuestion(s"Something went wrong WhenDidYouShareThisPost user: $user_id", TLWhenDidYouShareThisPost)
      }

  }

}
