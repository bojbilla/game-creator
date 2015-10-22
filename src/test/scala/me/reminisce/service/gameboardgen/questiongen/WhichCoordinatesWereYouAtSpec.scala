package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities.{FBLocation, FBPlace, FBPost}
import me.reminisce.service.gameboardgen.GameboardEntities.{GeolocationQuestion, TextPostSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhichCoordinatesWereYouAtSpec extends DatabaseTester("WhichCoordinatesWereYouAtSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserWhichCoordinatesWereYouAt"

  "WhichCoordinatesWereYouAt" must {
    "not create question when there is no post." in {
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhichCoordinatesWereYouAt.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Post not found : $itemId"))
    }

    "not create question when there is no location." in {
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val itemId = "This post does not exist"

      val fbPost = FBPost(postId = itemId, userId = userId)
      val selector = BSONDocument("userId" -> userId, "postId" -> itemId)
      Await.result(postsCollection.update(selector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhichCoordinatesWereYouAt.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Post has no place : $itemId"))
    }

    "create a valid question when the post and place is there." in {
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val userId = "TestUser"
      val itemId = "PostId"
      val postMessage = "Awesome Message"

      val latitude = 6.2
      val longitude = 45.13
      val location = FBLocation(None, None, latitude = latitude, longitude = longitude, None, None)
      val place = FBPlace(None, name = "SuperPlace", location = location, None)
      val selector = BSONDocument("userId" -> userId, "pageId" -> itemId)
      val fbPost = FBPost(postId = itemId, userId = userId, message = Some(postMessage), place = Some(place))
      Await.result(postsCollection.update(selector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhichCoordinatesWereYouAt.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))

      val finishedCreation = testProbe.receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(finishedCreation != null)
      assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

      val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
      assert(question.isInstanceOf[GeolocationQuestion])

      assert(question.asInstanceOf[GeolocationQuestion].subject.isDefined)
      val subject = question.asInstanceOf[GeolocationQuestion].subject.get
      val answer = question.asInstanceOf[GeolocationQuestion].answer

      assert(subject.isInstanceOf[TextPostSubject])
      assert(subject.asInstanceOf[TextPostSubject].text == fbPost.message.getOrElse(""))

      assert(answer.latitude == latitude)
      assert(answer.longitude == longitude)
    }
  }

}