package me.reminisce.gameboard.questions

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.analysis.DataTypes.{PostWhoCommented, PostWhoLiked, PostWhoReacted}
import me.reminisce.database.AnalysisEntities.UserSummary
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.FBComment.simpleReactionFromComment
import me.reminisce.database.MongoDBEntities._
import me.reminisce.gameboard.board.GameboardEntities.{CommentSubject, MultipleChoiceQuestion}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhoReactedToYourPostSpec extends QuestionTester("WhoReactedToYourPostSpec") {

  val userId = "TestUserWhoReactedToYourPost"

  "WhoReactedToYourPost" must {
    "not create question when there is no user summary." in {
      testWithDb {
        db =>
          val itemId = "This post does not exist"

          val actorRef = TestActorRef(WhoReactedToYourPost.props(db, PostWhoReacted))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough reactioners or non-reactioners."))
      }
    }

    "not create question when there is no post." in {
      testWithDb {
        db =>
          val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

          val itemId = "This post does not exist"

          val userSummary = UserSummary(userId = userId)
          Await.result(userSummariesCollection.update(userSummary, userSummary, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhoReactedToYourPost.props(db, PostWhoReacted))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough reactioners or non-reactioners."))
      }
    }

    "not create question when there is no reaction for post." in {
      testWithDb {
        db =>
          val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

          val itemId = "This post does exist"

          val userSummary = UserSummary(userId = userId)
          Await.result(userSummariesCollection.update(userSummary, userSummary, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val postsCollection = db[BSONCollection](MongoCollections.fbPosts)

          val fbPost = FBPost(postId = itemId, userId = userId)
          Await.result(postsCollection.update(fbPost, fbPost, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhoReactedToYourPost.props(db, PostWhoReacted))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough reactioners or non-reactioners."))
      }
    }

    "not create question when there is not enough non-reactioners for post." in {
      testWithDb {
        db =>
          val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

          val itemId = "This post does exist"

          val userSummary = UserSummary(userId = userId)
          Await.result(userSummariesCollection.update(userSummary, userSummary, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val postsCollection = db[BSONCollection](MongoCollections.fbPosts)

          val reactionerId = "LikerId"
          val reactionerName = "LikerName"
          val like = FBReaction(FBFrom(reactionerId, reactionerName), PostWhoLiked)
          val fbPost = FBPost(postId = itemId, userId = userId, reactions = Some(List(like)))
          Await.result(postsCollection.update(fbPost, fbPost, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhoReactedToYourPost.props(db, PostWhoReacted))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough reactioners or non-reactioners."))
      }
    }

    "create a valid question when the data is correctly setup." in {
      testWithDb {
        db =>
          val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

          val itemId = "Fresh post for this test"

          val commenters = (0 until 6).map {
            i =>
              val commenterId = s"CommenterId$i"
              val commenterName = s"CommenterName$i"
              FBComment(s"id$i", FBFrom(commenterId, commenterName), i, s"Message$i")
          }.toList

          val freshUser = userId + "Fresh"
          val userSummary = UserSummary(userId = freshUser, reactioners = commenters.map(simpleReactionFromComment).toSet)
          Await.result(userSummariesCollection.update(userSummary, userSummary, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val postsCollection = db[BSONCollection](MongoCollections.fbPosts)

          val message = "Who commented here?"
          val fbPost = FBPost(postId = itemId, userId = freshUser, comments = Some(List(commenters.head)), message = Some(message))
          Await.result(postsCollection.update(fbPost, fbPost, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          // Using comments here as it tests the most code
          val actorRef = TestActorRef(WhoReactedToYourPost.props(db, PostWhoCommented))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(freshUser, itemId))

          checkFinished[MultipleChoiceQuestion](testProbe) {
            question =>
              checkSubject[CommentSubject](question.subject) {
                subject =>
                  val answer = question.answer
                  val choices = question.choices
                  commenters.headOption match {
                    case Some(commenter) =>
                      assert(subject.comment == commenter.message)
                      assert(choices(answer).name == commenter.from.userName)
                      assert(subject.post == QuestionGenerator.subjectFromPost(fbPost))
                    case None =>
                      fail("No reactioner.")
                  }
              }
          }
      }
    }

  }
}