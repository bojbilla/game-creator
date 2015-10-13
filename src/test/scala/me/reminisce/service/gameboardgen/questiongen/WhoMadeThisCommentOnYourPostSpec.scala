package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import com.github.simplyscala.{MongodProps, MongoEmbedDatabase}
import com.typesafe.config.ConfigFactory
import me.reminisce.database.{MongoDatabaseService, DatabaseTestHelper}
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.gameboardgen.GameboardEntities.{CommentSubject, TextPostSubject, MultipleChoiceQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{FinishedQuestionCreation, NotEnoughData, CreateQuestion}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class WhoMadeThisCommentOnYourPostSpec extends TestKit(ActorSystem("WhoMadeThisCommentOnYourPostSpec", ConfigFactory.parseString("akka.loglevel = OFF")))
with MongoEmbedDatabase with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  var port = DatabaseTestHelper.getNewPort
  var mongoProps: MongodProps = mongoStart(port = port)
  val driver = new MongoDriver
  val connection = driver.connection(s"localhost:$port" :: Nil)
  val db = connection("mydb")

  val userId = "TestUserWhoMadeThisCommentOnYourPost"


  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    db.drop()
    mongoStop(mongoProps)
    driver.system.shutdown()
    DatabaseTestHelper.releasePort(port)
  }

  "WhoMadeThisCommentOnYourPost" must {
    "not create question when there is no post" in {
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhoMadeThisCommentOnYourPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Post not found : $itemId"))
    }


    "not create question when there is no comment for post." in {
      val itemId = "This post does exist"

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val fbPost = FBPost(postId = itemId, userId = userId)
      val postSelector = BSONDocument("userId" -> userId, "postId" -> itemId)
      Await.result(postsCollection.update(postSelector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoMadeThisCommentOnYourPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Post has no comment : $itemId"))
    }

    "not create question when there is not enough comment for post." in {
      val itemId = "This post does exist"

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val fromId = "FromId"
      val fromName = "FromName"
      val from = FBFrom(fromId, fromName)
      val comment = FBComment("commentId", from, 0, "hello")
      val fbPost = FBPost(postId = itemId, userId = userId, comments = Some(List(comment)))
      val postSelector = BSONDocument("userId" -> userId, "postId" -> itemId)
      Await.result(postsCollection.update(postSelector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoMadeThisCommentOnYourPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Post has not enough comments : $itemId"))
    }

    "create a valid question when the data is correctly setup." in {
      val itemId = "Fresh post for this test"

      val comments = (0 until 4).map {
        i =>
          val fromId = s"FromId$i"
          val fromName = s"FromName$i"
          val from = FBFrom(fromId, fromName)
          FBComment(s"commentId$i", from, 0, s"hello$i")
      }.toList

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val message = "Who liked this ?"
      val fbPost = FBPost(postId = itemId, userId = userId, comments = Some(comments), message = Some(message))
      val postSelector = BSONDocument("userId" -> userId, "postId" -> itemId)
      Await.result(postsCollection.update(postSelector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoMadeThisCommentOnYourPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)

      val finishedCreation = receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(finishedCreation != null)
      assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

      val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
      assert(question.isInstanceOf[MultipleChoiceQuestion])

      assert(question.asInstanceOf[MultipleChoiceQuestion].subject.isDefined)
      val subject = question.asInstanceOf[MultipleChoiceQuestion].subject.get
      val choices = question.asInstanceOf[MultipleChoiceQuestion].choices
      assert(subject.isInstanceOf[CommentSubject])
      val postSubject = subject.asInstanceOf[CommentSubject].post
      assert(postSubject.isInstanceOf[TextPostSubject])
      assert(postSubject.asInstanceOf[TextPostSubject].text == fbPost.message.getOrElse(""))
      val chosenComments = choices.map(c => c.name)
      val originalComments = comments.map(c => c.from.userName)
      chosenComments.foreach {
        c => assert(originalComments.contains(c))
      }
    }
  }

}