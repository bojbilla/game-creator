package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import com.github.nscala_time.time.Imports._
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.service.gameboardgen.GameboardEntities.{PageSubject, TimelineQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhenDidYouLikeThisPageSpec extends DatabaseTester("WhenDidYouLikeThisPageSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserWhenDidYouLikeThisPage"

  "WhenDidYouLikeThisPage" must {
    "not create question when there is no like." in {
      val itemId = "This page does not exist"

      val actorRef = TestActorRef(WhenDidYouLikeThisPage.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsgType[NotEnoughData]
    }

    "not create question when there is no page." in {
      val itemId = "This oage does not exist"

      val selectorLike = BSONDocument("userId" -> userId, "pageId" -> itemId)
      val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      val likedTime = DateTime.now
      val pageLike = FBPageLike(None, userId, itemId, likedTime)
      Await.result(pageLikesCollection.update(selectorLike, pageLike, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhenDidYouLikeThisPage.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))
      testProbe.expectMsgType[NotEnoughData]
    }

    "create a valid question when the data is there." in {
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)

      val itemId = s"PageId"

      val selectorPage = BSONDocument("pageId" -> itemId)
      val page = FBPage(None, itemId, Some(s"Cool page with id ID"), None, 1)
      Await.result(pagesCollection.update(selectorPage, page, upsert = true), Duration(10, TimeUnit.SECONDS))

      val selectorLike = BSONDocument("userId" -> userId, "pageId" -> itemId)
      val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      val likedTime = DateTime.now
      val pageLike = FBPageLike(None, userId, itemId, likedTime)
      Await.result(pageLikesCollection.update(selectorLike, pageLike, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhenDidYouLikeThisPage.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestion(userId, itemId))

      val finishedCreation = testProbe.receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(finishedCreation != null)
      assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

      val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
      assert(question.isInstanceOf[TimelineQuestion])

      assert(question.asInstanceOf[TimelineQuestion].subject.isDefined)
      val subject = question.asInstanceOf[TimelineQuestion].subject.get
      val answer = question.asInstanceOf[TimelineQuestion].answer

      assert(subject.isInstanceOf[PageSubject])
      assert(subject.asInstanceOf[PageSubject].name == page.name.getOrElse(""))
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
      assert(likedTime.toString(formatter) == answer)
    }
  }

}