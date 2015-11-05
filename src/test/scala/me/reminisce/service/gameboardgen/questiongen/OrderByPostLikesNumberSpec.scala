package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.{OrderQuestion, TextPostSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestionWithMultipleItems, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class OrderByPostLikesNumberSpec extends QuestionTester("OrderByPostLikesNumberSpec") {

  val userId = "TestUserOrderByPostLikesNumber"

  "OrderByPostLikesNumber" must {
    "not create question when there is not enough data." in {
      val db = newDb()
      val itemIds = List("This user does not exist")

      val actorRef = TestActorRef(OrderByPostLikesNumber.props(db))
      val testProbe = TestProbe()
      testProbe.send(actorRef, CreateQuestionWithMultipleItems(userId, itemIds))
      testProbe.expectMsg(NotEnoughData(s"Not enough posts in list."))
    }

    "create a valid question when the data is there." in {
      val db = newDb()
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val postsNumber = QuestionGenerationConfig.orderingItemsNumber

      val itemIds: List[String] = (1 to postsNumber).map {
        case nb => s"Post$nb"
      }.toList

      val posts = (0 until postsNumber).map {
        case nb =>
          FBPost(None, userId, itemIds(nb), Some(s"Cool post $nb"), likesCount = Some(nb))
      }.toList

      (0 until postsNumber) foreach {
        case nb =>
          Await.result(postsCollection.save(posts(nb), safeLastError), Duration(10, TimeUnit.SECONDS))
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