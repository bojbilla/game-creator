package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{TimeUnit, TimelineQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Random, Success}


object WhenDidYouShareThisPost {

  def props(database: DefaultDB): Props =
    Props(new WhenDidYouShareThisPost(database))
}

class WhenDidYouShareThisPost(db: DefaultDB) extends QuestionGenerator {
  def receive = {
    case CreateQuestion(userId, itemId) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      val client = sender()
      val query = BSONDocument(
        "userId" -> userId,
        "postId" -> itemId
      )
      val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      postCollection.find(query).one[FBPost].onComplete {
        case Success(postOpt) =>
          val post = postOpt.get
          val dateString = post.createdTime.get
          val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
          val actualDate = formatter.parseDateTime(dateString)
          val unit = TimeUnit.Day
          val step = 1
          val threshold = 3
          val rangeSize = 30
          val rangeBefore = Random.nextInt(rangeSize)
          val min = actualDate - rangeBefore.days
          val max = actualDate + (rangeSize - rangeBefore).days
          val postSubject = subjectFromPost(post)
          val tlQuestion = TimelineQuestion(userId, Timeline, TLWhenDidYouShareThisPost, Some(postSubject),
            actualDate.toString(formatter), min.toString(formatter), max.toString(formatter), min.toString(formatter),
            unit, step, threshold)
          client ! FinishedQuestionCreation(tlQuestion)
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
      }
  }

}
