package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.nscala_time.time.Imports._
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.config.ConfigFactory
import me.reminisce.database.{DatabaseTestHelper, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.service.gameboardgen.GameboardEntities.{PageSubject, TimelineQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.joda.time.DateTime
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration


class WhenDidYouLikeThisPageSpec extends TestKit(ActorSystem("WhenDidYouLikeThisPageSpec", ConfigFactory.parseString("akka.loglevel = OFF")))
with MongoEmbedDatabase with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  var port = DatabaseTestHelper.getNewPort
  var mongoProps: MongodProps = mongoStart(port = port)
  val driver = new MongoDriver
  val connection = driver.connection(s"localhost:$port" :: Nil)
  val db = connection("mydb")

  val userId = "TestUserWhenDidYouLikeThisPage"


  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    db.drop()
    mongoStop(mongoProps)
    driver.system.shutdown()
    DatabaseTestHelper.releasePort(port)
  }

  "WhenDidYouLikeThisPage" must {
    "not create question when there is no like." in {
      val itemId = "This page does not exist"

      val actorRef = TestActorRef(WhenDidYouLikeThisPage.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Pagelike not found : $userId likes $itemId"))
    }

    "not create question when there is no page." in {
      val itemId = "This oage does not exist"

      val selectorLike = BSONDocument("userId" -> userId, "pageId" -> itemId)
      val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      val likedTime = DateTime.now
      val pageLike = FBPageLike(None, userId, itemId, likedTime)
      Await.result(pageLikesCollection.update(selectorLike, pageLike, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhenDidYouLikeThisPage.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Page not found : $itemId"))
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
      actorRef ! CreateQuestion(userId, itemId)

      val finishedCreation = receiveOne(Duration(10, TimeUnit.SECONDS))
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