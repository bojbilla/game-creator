package service.question_generators

import akka.actor.Props
import entities.Entities.{Question, TimelineQuestion, MultipleChoiceQuestion}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import reactivemongo.api.DefaultDB
import reactivemongo.bson.BSONDocument
import server.domain.RestMessage
import service.GameGenerator.{FailedToCreateQuestion, CreateQuestion}
import com.github.nscala_time.time.Imports._
import service.question_generators.WhenDidYouShareThisPost.CreatedWhenDidYouShareThisPost
import scala.collection.immutable.StringOps
import scala.util.Random

/**
 * Created by roger on 20/11/14.
 */

object WhenDidYouShareThisPost {

  def props(database: DefaultDB): Props =
    Props(new WhenDidYouShareThisPost(database))

  case class CreatedWhenDidYouShareThisPost(tl: TimelineQuestion) extends RestMessage
}
class WhenDidYouShareThisPost(db: DefaultDB) extends PostQuestionGenerator(db) {
  def receive = {
    case CreateQuestion(user_id) =>
      val client = sender()
      val query = BSONDocument(
        "user_id" -> user_id,
        "message" -> BSONDocument("$exists" -> "true")
      )
      log.info("we do stuff")
      val question = getDocument(db, collection, query).map{
        postO => postO.map{
          post =>
            log.info("we did stuff")

            val q = Question("When did you share this post?", post.message)
//            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd hh:mm:ss")
            val index = post.created_time.get.indexOf('T')

            val dateString = post.created_time.get.substring(0, index)
            log.info(s"date: $dateString")
            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
//            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd G 'at' HH:mm:ss z")

            val range = 60
            val lower = range - Random.nextInt(range)
            val upper = range - lower
            val actualDate = formatter.parseDateTime(dateString)
//              formatter.parseDateTime(post.created_time.get)
            log.info("we formatted stuff")

            val minDate = actualDate - lower.days
            val maxDate = actualDate + upper.days
            TimelineQuestion("someId", user_id, q, minDate, maxDate, 10, actualDate)
        }
      }
      question.map{
        case Some(q) =>
          client ! CreatedWhenDidYouShareThisPost(q)
        case None => client ! FailedToCreateQuestion(s"Something went wrong WhenDidYouShareThisPost user: $user_id")

      }


  }

}
