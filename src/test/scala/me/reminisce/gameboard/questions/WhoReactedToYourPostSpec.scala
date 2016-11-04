package me.reminisce.gameboard.questions

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.AnalysisEntities.UserSummary
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities._
import me.reminisce.gameboard.board.GameboardEntities.{MultipleChoiceQuestion, TextPostSubject}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhoReactedToYourPostSpec extends QuestionTester("WhichPageDidYouLikeSpec") {

  val userId = "TestUserWhoReactedToYourPost"

  "WhoReactedToYourPost" must {
    "not create question when there is no user summary." in {
      testWithDb {
        db =>
          val itemId = "This post does not exist"

          val actorRef = TestActorRef(WhoReactedToYourPost.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough likers or non-likers."))
      }
    }

    "not create question when there is no post." in {
      testWithDb {
        db =>
          val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

          val itemId = "This post does not exist"

          val userSummary = UserSummary(userId = userId)
          Await.result(userSummariesCollection.update(userSummary, userSummary, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhoReactedToYourPost.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough likers or non-likers."))
      }
    }

    "not create question when there is no likes for post." in {
      testWithDb {
        db =>
          val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

          val itemId = "This post does exist"

          val userSummary = UserSummary(userId = userId)
          Await.result(userSummariesCollection.update(userSummary, userSummary, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val postsCollection = db[BSONCollection](MongoCollections.fbPosts)

          val fbPost = FBPost(postId = itemId, userId = userId)
          Await.result(postsCollection.update(fbPost, fbPost, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhoReactedToYourPost.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough likers or non-likers."))
      }
    }

    "not create question when there is not enough non-likers for post." in {
      testWithDb {
        db =>
          val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

          val itemId = "This post does exist"

          val userSummary = UserSummary(userId = userId)
          Await.result(userSummariesCollection.update(userSummary, userSummary, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val postsCollection = db[BSONCollection](MongoCollections.fbPosts)

          val likerId = "LikerId"
          val likerName = "LikerName"
          val like = FBReaction(likerId, likerName, "")
          val fbPost = FBPost(postId = itemId, userId = userId, reactions = Some(List(like)))
          Await.result(postsCollection.update(fbPost, fbPost, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhoReactedToYourPost.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough likers or non-likers."))
      }
    }

    "create a valid question when the data is correctly setup." in {
      testWithDb {
        db =>
          val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

          val itemId = "Fresh post for this test"

          val likers = (0 until 6).map {
            i =>
              val likerId = s"LikerId$i"
              val likerName = s"LikerName$i"
              FBReaction(likerId, likerName, "")
          }.toList

          val freshUser = userId + "Fresh"
          val userSummary = UserSummary(userId = freshUser, reactioners = likers.toSet)
          Await.result(userSummariesCollection.update(userSummary, userSummary, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val postsCollection = db[BSONCollection](MongoCollections.fbPosts)

          val message = "Who liked this ?"
          val fbPost = FBPost(postId = itemId, userId = freshUser, reactions = Some(List(likers.head)), message = Some(message))
          Await.result(postsCollection.update(fbPost, fbPost, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhoReactedToYourPost.props(db))
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
}