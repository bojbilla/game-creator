package me.reminisce.gameboard.questions

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.MongoDBEntities.FBPage
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities.{OrderQuestion, PageSubject}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestionWithMultipleItems, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class OrderByPageLikesSpec extends QuestionTester("OrderByPageLikesSpec") {

  val userId = "TestUserOrderByPageLikes"

  "OderByPageLikes" must {
    "not create question when there is not enough data." in {
      testWithDb {
        db =>
          val itemIds = List("This User does not exist")

          val actorRef = TestActorRef(OrderByPageLikes.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))
          testProbe.expectMsg(NotEnoughData(s"Not enough pages in list."))
      }
    }

    "create a valid question when the data is there." in {
      testWithDb {
        db =>
          val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)

          val pagesNumber = QuestionGenerationConfig.orderingItemsNumber

          val itemIds: List[String] = (1 to pagesNumber).map {
            nb => s"Page$nb"
          }.toList

          val pages = (0 until pagesNumber).map {
            nb =>
              FBPage(None, itemIds(nb), Some(s"Cool page with id $nb"), None, nb)
          }.toList

          (0 until pagesNumber) foreach {
            nb =>
              val selector = BSONDocument("pageId" -> pages(nb))
              Await.result(pagesCollection.update(selector, pages(nb), WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          val actorRef = TestActorRef(OrderByPageLikes.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))

          checkFinished[OrderQuestion](testProbe) {
            question =>
              orderCheck[PageSubject](question) {
                case (subject, nb) =>
                  assert(subject.name == pages(nb).name.getOrElse(""))
              }
          }
      }
    }
  }

}