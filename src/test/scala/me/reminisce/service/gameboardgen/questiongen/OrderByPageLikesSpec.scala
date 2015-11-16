package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPage
import me.reminisce.service.gameboardgen.GameboardEntities.{OrderQuestion, PageSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestionWithMultipleItems, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class OrderByPageLikesSpec extends QuestionTester("OrderByPageLikesSpec") {

  val userId = "TestUserOrderByPageLikes"

  "OderByPageLikes" must {
    "not create question when there is not enough data." in {
      val db = newDb()
      val itemIds = List("This user does not exist")

      val actorRef = TestActorRef(OrderByPageLikes.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))
      testProbe.expectMsg(NotEnoughData(s"Not enough pages in list."))
    }

    "create a valid question when the data is there." in {
      val db = newDb()
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)

      val pagesNumber = QuestionGenerationConfig.orderingItemsNumber

      val itemIds: List[String] = (1 to pagesNumber).map {
        case nb => s"Page$nb"
      }.toList

      val pages = (0 until pagesNumber).map {
        case nb =>
          FBPage(None, itemIds(nb), Some(s"Cool page with id $nb"), None, nb)
      }.toList

      (0 until pagesNumber) foreach {
        case nb =>
          val selector = BSONDocument("pageId" -> pages(nb))
          Await.result(pagesCollection.update(selector, pages(nb), safeLastError, upsert = true), Duration(10, TimeUnit.SECONDS))
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