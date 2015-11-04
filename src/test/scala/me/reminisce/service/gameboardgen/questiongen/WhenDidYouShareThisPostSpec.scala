package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import com.github.nscala_time.time.Imports._
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.{TextPostSubject, TimelineQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhenDidYouShareThisPostSpec extends DatabaseTester("WhenDidYouShareThisPostSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserWhenDidYouShareThisPost"

  "WhenDidYouShareThisPost" must {
    "not create question when there is no post." in {
      val db = newDb()
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhenDidYouShareThisPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Post not found : $itemId"))
      db.drop()
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

      val finishedCreation = testProbe.receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(finishedCreation != null)
      assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

      val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
      assert(question.isInstanceOf[TimelineQuestion])

      assert(question.asInstanceOf[TimelineQuestion].subject.isDefined)
      val subject = question.asInstanceOf[TimelineQuestion].subject.get
      val answer = question.asInstanceOf[TimelineQuestion].answer

      assert(subject.isInstanceOf[TextPostSubject])
      assert(subject.asInstanceOf[TextPostSubject].text == fbPost.message.getOrElse(""))

      assert(postedTime == answer)
      db.drop()
    }
  }

}