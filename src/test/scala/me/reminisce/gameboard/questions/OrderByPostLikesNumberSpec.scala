package me.reminisce.gameboard.questions

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.MongoDBEntities.FBPost
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities.{OrderQuestion, TextPostSubject}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestionWithMultipleItems, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class OrderByPostLikesNumberSpec extends QuestionTester("OrderByPostLikesNumberSpec") {

  val userId = "TestUserOrderByPostLikesNumber"

  "OrderByPostLikesNumber" must {
    "not create question when there is not enough data." in {
      testWithDb {
        db =>
          val itemIds = List("This user does not exist")

          val actorRef = TestActorRef(OrderByPostLikesNumber.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))
          testProbe.expectMsg(NotEnoughData(s"Not enough posts in list."))
      }
    }

    "create a valid question when the data is there." in {
      testWithDb {
        db =>
          val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

          val postsNumber = QuestionGenerationConfig.orderingItemsNumber

          val itemIds: List[String] = (1 to postsNumber).map {
            nb => s"Post$nb"
          }.toList

          val posts = (0 until postsNumber).map {
            nb =>
              FBPost(None, userId, itemIds(nb), Some(s"Cool post $nb"), likesCount = Some(nb))
          }.toList

          (0 until postsNumber) foreach {
            nb =>
              Await.result(postsCollection.update(posts(nb), posts(nb), WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          val actorRef = TestActorRef(OrderByPostLikesNumber.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))

          checkFinished[OrderQuestion](testProbe) {
            question =>
              orderCheck[TextPostSubject](question) {
                case (subject, nb) =>
                  assert(subject.text == posts(nb).message.getOrElse(""))
              }
          }
      }
    }
  }

}