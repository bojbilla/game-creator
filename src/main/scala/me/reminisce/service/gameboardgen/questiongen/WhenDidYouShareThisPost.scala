package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{PostQuestion, TimelineQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FailedToCreateQuestion, FinishedQuestionCreation}
import org.joda.time.format.DateTimeFormat
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Success}


object WhenDidYouShareThisPost {

  def props(database: DefaultDB): Props =
    Props(new WhenDidYouShareThisPost(database))
}

class WhenDidYouShareThisPost(db: DefaultDB) extends QuestionGenerator {
  def receive = {
    case CreateQuestion(user_id, item_id) =>
      val client = sender()
      val query = BSONDocument(
        "user_id" -> user_id,
        "post_id" -> item_id
      )
      val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      postCollection.find(query).one[FBPost].onComplete {
        case Success(postOpt) =>
          val post = postOpt.get
          val index = post.created_time.get.indexOf('T')
          val dateString = post.created_time.get.substring(0, index)
          val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
          val actualDate = formatter.parseDateTime(dateString)
          val postInQuestion = postInQuestionFromPost(post)
          val question = PostQuestion(Timeline, TLWhenDidYouShareThisPost, postInQuestion, None)
          val tlQuestion = TimelineQuestion(user_id, question, actualDate)
          client ! FinishedQuestionCreation(tlQuestion)
        case Failure(e) =>
          client ! FailedToCreateQuestion(s"Could not reach database : $e.", TLWhenDidYouShareThisPost)
      }
  }

}
