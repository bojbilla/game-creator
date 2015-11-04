package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.mongodb.StatsEntities.UserStats
import me.reminisce.service.gameboardgen.GameboardEntities.{MultipleChoiceQuestion, TextPostSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhoLikedYourPostSpec extends DatabaseTester("WhichPageDidYouLikeSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserWhoLikedYourPost"

  "WhoLikedYourPost" must {
    "not create question when there is no user statistics." in {
      val db = newDb()
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsgType[NotEnoughData]
      db.drop()
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
      testProbe.expectMsgType[NotEnoughData]
      db.drop()
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
      testProbe.expectMsgType[NotEnoughData]
      db.drop()
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
      testProbe.expectMsgType[NotEnoughData]
      db.drop()
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

      val finishedCreation = testProbe.receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(finishedCreation != null)
      assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

      val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
      assert(question.isInstanceOf[MultipleChoiceQuestion])

      assert(question.asInstanceOf[MultipleChoiceQuestion].subject.isDefined)
      val subject = question.asInstanceOf[MultipleChoiceQuestion].subject.get
      val answer = question.asInstanceOf[MultipleChoiceQuestion].answer
      val choices = question.asInstanceOf[MultipleChoiceQuestion].choices
      assert(subject.isInstanceOf[TextPostSubject])
      assert(subject.asInstanceOf[TextPostSubject].text == fbPost.message.getOrElse(""))
      assert(choices(answer).name == likers.head.userName)
      db.drop()
    }

  }
}