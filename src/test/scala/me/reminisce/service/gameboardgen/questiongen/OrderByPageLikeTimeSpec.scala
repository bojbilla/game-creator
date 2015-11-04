package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.service.gameboardgen.GameboardEntities.{OrderQuestion, PageSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestionWithMultipleItems, FinishedQuestionCreation, NotEnoughData}
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class OrderByPageLikeTimeSpec extends DatabaseTester("OrderByPageLikeTimeSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserOrderByPageLikeTime"

  "OrderByPageLikeTime" must {
    "not create question when there is not enough data." in {
      val db = newDb()
      val itemIds = List("This user does not exist")

      val actorRef = TestActorRef(OrderByPageLikeTime.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))
      testProbe.expectMsg(NotEnoughData(s"Did not find enough page-likes."))
    }

    "create a valid question when the data is there." in {
      val db = newDb()
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
      val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)

      val pagesNumber = QuestionGenerationConfig.orderingItemsNumber

      val itemIds: List[String] = (1 to pagesNumber).map {
        case nb => s"Page$nb"
      }.toList

      val pages = (0 until pagesNumber).map {
        case nb =>
          FBPage(None, itemIds(nb), Some(s"Cool page with id $nb"), None, nb)
      }.toList

      val pageLikes = (0 until pagesNumber).map {
        case nb =>
          FBPageLike(None, userId, itemIds(nb), new DateTime(nb))
      }

      (0 until pagesNumber) foreach {
        case nb =>
          Await.result(pagesCollection.save(pages(nb), safeLastError), Duration(10, TimeUnit.SECONDS))
      }

      (0 until pagesNumber) foreach {
        case nb =>
          Await.result(pageLikesCollection.save(pageLikes(nb), safeLastError), Duration(10, TimeUnit.SECONDS))
      }

      val actorRef = TestActorRef(OrderByPageLikeTime.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))

      val finishedCreation = testProbe.receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(finishedCreation != null)
      assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

      val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
      assert(question.isInstanceOf[OrderQuestion])

      val subjectWithIds = question.asInstanceOf[OrderQuestion].choices
      val answer = question.asInstanceOf[OrderQuestion].answer

      (0 until pagesNumber).foreach {
        case nb =>
          val a = answer(nb)
          val subject = subjectWithIds.filter(elm => elm.uId == a).head.subject
          assert(subject.isInstanceOf[PageSubject])
          assert(subject.asInstanceOf[PageSubject].name == pages(nb).name.getOrElse(""))
      }
    }
  }
}
