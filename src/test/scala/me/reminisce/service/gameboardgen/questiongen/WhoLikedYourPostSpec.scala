package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.config.ConfigFactory
import me.reminisce.database.{DatabaseTestHelper, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.gameboardgen.GameboardEntities.{MultipleChoiceQuestion, TextPostSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class WhoLikedYourPostSpec extends TestKit(ActorSystem("WhoLikedYourPostSpec", ConfigFactory.parseString("akka.loglevel = OFF")))
with MongoEmbedDatabase with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  var port = DatabaseTestHelper.getNewPort
  var mongoProps: MongodProps = mongoStart(port = port)
  val driver = new MongoDriver
  val connection = driver.connection(s"localhost:$port" :: Nil)
  val db = connection("mydb")

  val userId = "TestUserWhoLikedYourPost"


  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    db.drop()
    mongoStop(mongoProps)
    driver.system.shutdown()
    DatabaseTestHelper.releasePort(port)
  }

  "WhoLikedYourPost" must {
    "not create question when there is no user statistics." in {
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Strangely there is no userStats."))
    }

    "not create question when there is no post." in {
      val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      val itemId = "This post does not exist"

      val userStats = UserStats(userId = userId)
      val selector = BSONDocument("userId" -> userId)
      Await.result(userStatsCollection.update(selector, userStats, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Post not found : $itemId"))
    }

    "not create question when there is no likes for post." in {
      val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      val itemId = "This post does exist"

      val userStats = UserStats(userId = userId)
      val userStatsSelector = BSONDocument("userId" -> userId)
      Await.result(userStatsCollection.update(userStatsSelector, userStats, upsert = true), Duration(10, TimeUnit.SECONDS))

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val fbPost = FBPost(postId = itemId, userId = userId)
      val postSelector = BSONDocument("userId" -> userId, "postId" -> itemId)
      Await.result(postsCollection.update(postSelector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"No likes on post : $itemId"))
    }

    "not create question when there is not enough non-likers for post." in {
      val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      val itemId = "This post does exist"

      val userStats = UserStats(userId = userId)
      val userStatsSelector = BSONDocument("userId" -> userId)
      Await.result(userStatsCollection.update(userStatsSelector, userStats, upsert = true), Duration(10, TimeUnit.SECONDS))

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val likerId = "LikerId"
      val likerName = "LikerName"
      val like = FBLike(likerId, likerName)
      val fbPost = FBPost(postId = itemId, userId = userId, likes = Some(List(like)))
      val postSelector = BSONDocument("userId" -> userId, "postId" -> itemId)
      Await.result(postsCollection.update(postSelector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Not enough non likers for post $itemId and user $userId"))
    }

    "create a valid question when the data is correctly setup." in {
      val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      val itemId = "Fresh post for this test"

      val likers = (0 until 4).map {
        i =>
          val likerId = s"LikerId$i"
          val likerName = s"LikerName$i"
          FBLike(likerId, likerName)
      }.toList

      val userStats = UserStats(userId = userId, likers = likers.toSet)
      val userStatsSelector = BSONDocument("userId" -> userId)
      Await.result(userStatsCollection.update(userStatsSelector, userStats, upsert = true), Duration(10, TimeUnit.SECONDS))

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val message = "Who liked this ?"
      val fbPost = FBPost(postId = itemId, userId = userId, likes = Some(List(likers.head)), message = Some(message))
      val postSelector = BSONDocument("userId" -> userId, "postId" -> itemId)
      Await.result(postsCollection.update(postSelector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoLikedYourPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)

      val finishedCreation = receiveOne(Duration(10, TimeUnit.SECONDS))
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
    }
  }

}