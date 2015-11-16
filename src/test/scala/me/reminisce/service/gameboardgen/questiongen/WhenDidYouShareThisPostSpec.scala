package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.{TextPostSubject, TimelineQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhenDidYouShareThisPostSpec extends QuestionTester("WhenDidYouShareThisPostSpec") {

  val userId = "TestUserWhenDidYouShareThisPost"

  "WhenDidYouShareThisPost" must {
    "not create question when there is no post." in {
      val db = newDb()
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhenDidYouShareThisPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Post not found : $itemId"))
    }

    "create a valid question when the post is there." in {
      val db = newDb()
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val itemId = "PostId"
      val postMessage = "Awesome Message"

      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
      val postedTime = DateTime.now.toString(formatter)
      val fbPost = FBPost(postId = itemId, userId = userId, attachments = None, message = Some(postMessage),
        createdTime = Some(postedTime))
      Await.result(postsCollection.save(fbPost, safeLastError), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhenDidYouShareThisPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))

      checkFinished[TimelineQuestion](testProbe) {
        question =>
          checkSubject[TextPostSubject](question.subject) {
            subject =>
              val answer = question.answer
              assert(subject.text == fbPost.message.getOrElse(""))
              assert(postedTime == answer)
          }
      }
    }
  }

}