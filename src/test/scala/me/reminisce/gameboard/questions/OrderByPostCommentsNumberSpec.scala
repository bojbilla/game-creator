package me.reminisce.gameboard.questions

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities._
import me.reminisce.gameboard.board.GameboardEntities.{OrderQuestion, TextPostSubject}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestionWithMultipleItems, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class OrderByPostCommentsNumberSpec extends QuestionTester("OrderByPostCommentsNumberSpec") {

  val userId = "TestUserOrderByPostCommentsNumber"

  "OrderByPostCommentsNumber" must {
    "not create question when there is not enough data." in {
      testWithDb {
        db =>
          val itemIds = List("This user does not exist")

          val actorRef = TestActorRef(OrderByPostCommentsNumber.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))
          testProbe.expectMsg(NotEnoughData(s"Not enough posts in list."))
      }
    }

    "create a valid question when the data is there." in {
      testWithDb {
        db =>
          val pagesCollection = db[BSONCollection](MongoCollections.fbPosts)

          val postsNumber = QuestionGenerationConfig.orderingItemsNumber

          val itemIds: List[String] = (1 to postsNumber).map {
            nb => s"Post$nb"
          }.toList

          val posts = (0 until postsNumber).map {
            nb =>
              FBPost(None, userId, itemIds(nb), Some(s"Cool post $nb"), commentsCount = Some(nb))
          }.toList

          (0 until postsNumber) foreach {
            nb =>
              Await.result(pagesCollection.update(posts(nb), posts(nb), WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))
          }
          val testProbe = TestProbe()
          val actorRef = TestActorRef(OrderByPostCommentsNumber.props(db))
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