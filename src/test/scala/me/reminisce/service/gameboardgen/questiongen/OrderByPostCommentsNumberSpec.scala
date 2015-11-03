package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.{DatabaseTester, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.gameboardgen.GameboardEntities.{OrderQuestion, TextPostSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestionWithMultipleItems, FinishedQuestionCreation, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@DoNotDiscover
class OrderByPostCommentsNumberSpec extends DatabaseTester("OrderByPostCommentsNumberSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  val userId = "TestUserOrderByPostCommentsNumber"

  "OrderByPostCommentsNumber" must {
    "not create question when there is not enough data." in {
      val itemIds = List("This user does not exist")

      val actorRef = TestActorRef(OrderByPostCommentsNumber.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))
      testProbe.expectMsgType[NotEnoughData]
    }

    "create a valid question when the data is there." in {
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val postsNumber = QuestionGenerationConfig.orderingItemsNumber

      val itemIds: List[String] = (1 to postsNumber).map {
        case nb => s"Post$nb"
      }.toList

      val posts = (0 until postsNumber).map {
        case nb =>
          FBPost(None, userId, itemIds(nb), Some(s"Cool post $nb"), commentsCount = Some(nb))
      }.toList

      (0 until postsNumber) foreach {
        case nb =>
          val selector = BSONDocument("userId" -> userId, "postId" -> itemIds(nb))
          Await.result(pagesCollection.update(selector, posts(nb), upsert = true), Duration(10, TimeUnit.SECONDS))
      }
      val testProbe = TestProbe()
      val actorRef = TestActorRef(OrderByPostCommentsNumber.props(db))
      testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))

      val finishedCreation = testProbe.receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(finishedCreation != null)
      assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

      val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
      assert(question.isInstanceOf[OrderQuestion])

      val subjectWithIds = question.asInstanceOf[OrderQuestion].choices
      val answer = question.asInstanceOf[OrderQuestion].answer

      (0 until postsNumber).foreach {
        case nb =>
          val a = answer(nb)
          val subject = subjectWithIds.filter(elm => elm.uId == a).head.subject
          assert(subject.isInstanceOf[TextPostSubject])
          assert(subject.asInstanceOf[TextPostSubject].text == posts(nb).message.getOrElse(""))
      }
    }
  }

}