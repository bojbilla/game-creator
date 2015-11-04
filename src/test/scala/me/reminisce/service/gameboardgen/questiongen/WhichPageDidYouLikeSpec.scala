package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.service.gameboardgen.GameboardEntities.MultipleChoiceQuestion
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhichPageDidYouLikeSpec extends DatabaseTester("WhichPageDidYouLikeSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserWhichPageDidYouLike"

  "WhichPageDidYouLike" must {
    "not create question when there is not enough data." in {
      val db = newDb()
      val itemId = "This user does not exist"

      val actorRef = TestActorRef(WhichPageDidYouLike.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsg(NotEnoughData(s"Page not found. $itemId"))
      db.drop()
    }

    "create a valid question when the data is there." in {
      val db = newDb()
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)

      val pagesNumber = 4 // MC question

      val itemIds: List[String] = (1 to pagesNumber).map {
        case nb => s"Page$nb"
      }.toList

      val pages = (0 until pagesNumber).map {
        case nb =>
          FBPage(None, itemIds(nb), Some(s"Cool page with id $nb"), None, nb)
      }.toList

      (0 until pagesNumber) foreach {
        case nb =>
          Await.result(pagesCollection.save(pages(nb), safeLastError), Duration(10, TimeUnit.SECONDS))
      }

      val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      val pageLike = FBPageLike(None, userId, itemIds.head, DateTime.now)
      Await.result(pageLikesCollection.save(pageLike, safeLastError), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhichPageDidYouLike.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemIds.head))

      val finishedCreation = testProbe.receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(finishedCreation != null)
      assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

      val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
      assert(question.isInstanceOf[MultipleChoiceQuestion])

      val possibilitiesIds = question.asInstanceOf[MultipleChoiceQuestion].choices.map {
        poss =>
          assert(poss.fbId.isDefined)
          poss.fbId.get
      }
      val answer = question.asInstanceOf[MultipleChoiceQuestion].answer

      assert(possibilitiesIds(answer) == pages.head.pageId)
      db.drop()
    }
  }

}