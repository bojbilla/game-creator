package me.reminisce.gameboard.questions

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDBEntities.FBPost
import me.reminisce.database.{MongoDBEntities, MongoDatabaseService}
import me.reminisce.gameboard.board.GameboardEntities
import me.reminisce.gameboard.board.GameboardEntities.{OrderQuestion, TextPostSubject}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestionWithMultipleItems, NotEnoughData}
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class OrderByPostTimeSpec extends QuestionTester("OrderByPostTimeSpec") {

  val userId = "TestUserOrderByPostTime"

  "OrderByPostTime" must {
    "not create question when there is not enough data." in {
      val db = newDb()
      val itemIds = List("This user does not exist")

      val actorRef = TestActorRef(OrderByPostTime.props(db))
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
          val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
          val date = new DateTime(nb + 1)
          val dateAsString = date.toString(formatter)
          FBPost(None, userId, itemIds(nb), Some(s"Cool post $nb"), createdTime = Some(dateAsString))
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