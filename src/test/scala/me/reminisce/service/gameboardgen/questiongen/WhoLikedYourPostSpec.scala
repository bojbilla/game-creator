package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.mongodb.StatsEntities.UserStats
import me.reminisce.service.gameboardgen.GameboardEntities.{MultipleChoiceQuestion, TextPostSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhoLikedYourPostSpec extends QuestionTester("WhichPageDidYouLikeSpec") {

  val userId = "TestUserWhoLikedYourPost"

  "WhoLikedYourPost" must {
    "not create question when there is no user statistics." in {
      val db = newDb()
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Strangely there is no userStats."))
    }

    "not create question when there is no post." in {
      val db = newDb()
      val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      val itemId = "This post does not exist"

      val userStats = UserStats(userId = userId)
      Await.result(userStatsCollection.save(userStats, safeLastError), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Post not found : $itemId"))
    }

    "not create question when there is no likes for post." in {
      val db = newDb()
      val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      val itemId = "This post does exist"

      val userStats = UserStats(userId = userId)
      Await.result(userStatsCollection.save(userStats, safeLastError), Duration(10, TimeUnit.SECONDS))

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val fbPost = FBPost(postId = itemId, userId = userId)
      Await.result(postsCollection.save(fbPost, safeLastError), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"No likes on post : $itemId"))
    }

    "not create question when there is not enough non-likers for post." in {
      val db = newDb()
      val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      val itemId = "This post does exist"

      val userStats = UserStats(userId = userId)
      Await.result(userStatsCollection.save(userStats, safeLastError), Duration(10, TimeUnit.SECONDS))

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val likerId = "LikerId"
      val likerName = "LikerName"
      val like = FBLike(likerId, likerName)
      val fbPost = FBPost(postId = itemId, userId = userId, likes = Some(List(like)))
      Await.result(postsCollection.save(fbPost, safeLastError), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Not enough non likers for post $itemId and user $userId"))
    }

    "create a valid question when the data is correctly setup." in {
      val db = newDb()
      val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      val itemId = "Fresh post for this test"

      val likers = (0 until 6).map {
        i =>
          val likerId = s"LikerId$i"
          val likerName = s"LikerName$i"
          FBLike(likerId, likerName)
      }.toList

      val freshUser = userId + "Fresh"
      val userStats = UserStats(userId = freshUser, likers = likers.toSet)
      Await.result(userStatsCollection.save(userStats, safeLastError), Duration(10, TimeUnit.SECONDS))

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val message = "Who liked this ?"
      val fbPost = FBPost(postId = itemId, userId = freshUser, likes = Some(List(likers.head)), message = Some(message))
      Await.result(postsCollection.save(fbPost, safeLastError), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(freshUser, itemId))

      checkFinished[MultipleChoiceQuestion](testProbe) {
        question =>
          checkSubject[TextPostSubject](question.subject) {
            subject =>
              val answer = question.answer
              val choices = question.choices
              assert(subject.text == fbPost.message.getOrElse(""))
              likers.headOption match {
                case Some(liker) =>
                  assert(choices(answer).name == liker.userName)
                case None =>
                  fail("No liker.")
              }
          }
      }
    }

  }
}