package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.gameboardgen.GameboardEntities.{CommentSubject, MultipleChoiceQuestion, TextPostSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhoMadeThisCommentOnYourPostSpec extends DatabaseTester("WhichPageDidYouLikeSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserWhoMadeThisCommentOnYourPost"

  "WhoMadeThisCommentOnYourPost" must {
    "not create question when there is no post" in {
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhoMadeThisCommentOnYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsgType[NotEnoughData]
    }


    "not create question when there is no comment for post." in {
      val itemId = "This post does exist"

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val fbPost = FBPost(postId = itemId, userId = userId)
      val postSelector = BSONDocument("userId" -> userId, "postId" -> itemId)
      Await.result(postsCollection.update(postSelector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoMadeThisCommentOnYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsgType[NotEnoughData]
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
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsgType[NotEnoughData]
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
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))

      val finishedCreation = testProbe.receiveOne(Duration(10, TimeUnit.SECONDS))
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