package me.reminisce.gameboard.questions

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.MongoDBEntities.{FBLocation, FBPlace, FBPost}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities.{GeolocationQuestion, TextPostSubject}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhichCoordinatesWereYouAtSpec extends QuestionTester("WhichCoordinatesWereYouAtSpec") {

  val userId = "TestUserWhichCoordinatesWereYouAt"

  "WhichCoordinatesWereYouAt" must {
    "not create question when there is no post." in {
      testWithDb {
        db =>
          val itemId = "This post does not exist"

          val actorRef = TestActorRef(WhichCoordinatesWereYouAt.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"Post has no place or post does not exist : $itemId"))
      }
    }

    "not create question when there is no location." in {
      testWithDb {
        db =>
          val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

          val itemId = "This post does not exist"

          val fbPost = FBPost(postId = itemId, userId = userId)
          Await.result(postsCollection.update(fbPost, fbPost, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhichCoordinatesWereYouAt.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"Post has no place or post does not exist : $itemId"))
      }
    }

    "create a valid question when the post and place is there." in {
      testWithDb {
        db =>
          val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

          val userId = "TestUser"
          val itemId = "PostId"
          val postMessage = "Awesome Message"

          val latitude = 6.2
          val longitude = 45.13
          val location = FBLocation(None, None, latitude = latitude, longitude = longitude, None, None)
          val place = FBPlace(None, name = "SuperPlace", location = location, None)
          val fbPost = FBPost(postId = itemId, userId = userId, message = Some(postMessage), place = Some(place))
          Await.result(postsCollection.update(fbPost, fbPost, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhichCoordinatesWereYouAt.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))

          checkFinished[GeolocationQuestion](testProbe) {
            question =>
              checkSubject[TextPostSubject](question.subject) {
                subject =>
                  val answer = question.answer

                  assert(subject.text == fbPost.message.getOrElse(""))

                  assert(answer.latitude == latitude)
                  assert(answer.longitude == longitude)
              }
          }
      }
    }
  }

}