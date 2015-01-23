package service.question_generators

import akka.actor.Props
import entities.Entities.{Question, TimelineQuestion, MultipleChoiceQuestion}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import reactivemongo.api.DefaultDB
import reactivemongo.bson.{BSONArray, BSONDocument}
import com.github.nscala_time.time.Imports._
import service.question_generators.QuestionGenerator.{FailedToCreateQuestion, FinishedQuestionCreation, CreateQuestion}
import scala.util.Random
import entities.Entities.SpecificQuestionType._

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
      val question = getDocument(db, collection, query).map{
        postO => postO.map{
          post =>
            val q = Question("WhenDidYouShareThisPost", Some(List(post.message.getOrElse(""))))
            val index = post.created_time.get.indexOf('T')

            val dateString = post.created_time.get.substring(0, index)
            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
            val range = 60
            val lower = Random.nextInt(range)
            val upper = range - lower
            val actualDate = formatter.parseDateTime(dateString)
            val minDate = lower
            val maxDate = upper
            TimelineQuestion(post.post_id, user_id, q, minDate, maxDate, 10, actualDate)
        }
      }
      question.map{
        case Some(q) =>
          client ! FinishedQuestionCreation(q)
        case None => client ! FailedToCreateQuestion(s"Something went wrong WhenDidYouShareThisPost user: $user_id", TLWhenDidYouShareThisPost)

      }


  }

}
