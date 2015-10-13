package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.config.ConfigFactory
import me.reminisce.database.{DatabaseTestHelper, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.service.gameboardgen.GameboardEntities.MultipleChoiceQuestion
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.joda.time.DateTime
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class WhichPageDidYouLikeSpec extends TestKit(ActorSystem("WhichPageDidYouLikeSpec", ConfigFactory.parseString("akka.loglevel = OFF")))
with MongoEmbedDatabase with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  var port = DatabaseTestHelper.getNewPort
  var mongoProps: MongodProps = mongoStart(port = port)
  val driver = new MongoDriver
  val connection = driver.connection(s"localhost:$port" :: Nil)
  val db = connection("mydb")

  val userId = "TestUserWhichPageDidYouLike"


  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    db.drop()
    mongoStop(mongoProps)
    driver.system.shutdown()
    DatabaseTestHelper.releasePort(port)
  }

  "WhichPageDidYouLike" must {
    "not create question when there is not enough data." in {
      val itemId = "This user does not exist"

      val actorRef = TestActorRef(WhichPageDidYouLike.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Page not found. $itemId"))
    }

    "create a valid question when the data is there." in {
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
          val selector = BSONDocument("pageId" -> itemIds(nb))
          Await.result(pagesCollection.update(selector, pages(nb), upsert = true), Duration(10, TimeUnit.SECONDS))
      }

      val selector = BSONDocument("userId" -> userId, "pageId" -> itemIds.head)
      val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      val pageLike = FBPageLike(None, userId, itemIds.head, DateTime.now)
      Await.result(pageLikesCollection.update(selector, pageLike, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhichPageDidYouLike.props(db))
      actorRef ! CreateQuestion(userId, itemIds.head)

      val finishedCreation = receiveOne(Duration(10, TimeUnit.SECONDS))
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

      pages.foreach {
        pge =>
          assert(possibilitiesIds.contains(pge.pageId))
      }

      assert(possibilitiesIds(answer) == pages.head.pageId)
    }
  }

}