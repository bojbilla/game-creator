package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.gameboardgen.GameboardEntities.{CommentSubject, MultipleChoiceQuestion, TextPostSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhoMadeThisCommentOnYourPostSpec extends DatabaseTester("WhichPageDidYouLikeSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserWhoMadeThisCommentOnYourPost"

  "WhoMadeThisCommentOnYourPost" must {
    "not create question when there is no post" in {
      val db = newDb()
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhoMadeThisCommentOnYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Post not found : $itemId"))
      db.drop()
    }


    "not create question when there is no comment for post." in {
      val db = newDb()
      val itemId = "This post does exist"

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val fbPost = FBPost(postId = itemId, userId = userId)
      Await.result(postsCollection.save(fbPost, safeLastError), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoMadeThisCommentOnYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Post has no comment : $itemId"))
      db.drop()
    }

    "not create question when there is not enough comment for post." in {
      val db = newDb()
      val itemId = "This post does exist"

      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val fromId = "FromId"
      val fromName = "FromName"
      val from = FBFrom(fromId, fromName)
      val comment = FBComment("commentId", from, 0, "hello")
      val fbPost = FBPost(postId = itemId, userId = userId, comments = Some(List(comment)))
      Await.result(postsCollection.save(fbPost, safeLastError), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhoMadeThisCommentOnYourPost.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Post has not enough comments : $itemId"))
      db.drop()
    }

    "create a valid question when the data is correctly setup." in {
      val db = newDb()
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
      Await.result(postsCollection.save(fbPost, safeLastError), Duration(10, TimeUnit.SECONDS))

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
      db.drop()
    }
  }

}