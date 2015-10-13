package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.config.ConfigFactory
import me.reminisce.database.{DatabaseTestHelper, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities.FBPage
import me.reminisce.service.gameboardgen.GameboardEntities.{OrderQuestion, PageSubject}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestionWithMultipleItems, FinishedQuestionCreation, NotEnoughData}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class OrderByPageLikesSpec extends TestKit(ActorSystem("OrderByPageLikesSpec", ConfigFactory.parseString("akka.loglevel = OFF")))
with MongoEmbedDatabase with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  var port = DatabaseTestHelper.getNewPort
  var mongoProps: MongodProps = mongoStart(port = port)
  val driver = new MongoDriver
  val connection = driver.connection(s"localhost:$port" :: Nil)
  val db = connection("mydb")
  val userId = "TestUserOrderByPageLikes"


  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    db.drop()
    mongoStop(mongoProps)
    driver.system.shutdown()
    DatabaseTestHelper.releasePort(port)
  }

  "OderByPageLikes" must {
    "not create question when there is not enough data." in {
      val itemIds = List("This user does not exist")

      val actorRef = TestActorRef(OrderByPageLikes.props(db))
      actorRef ! CreateQuestionWithMultipleItems(userId, itemIds)
      expectMsg(NotEnoughData(s"Not enough pages in list."))
    }

    "create a valid question when the data is there." in {
      import scala.concurrent.ExecutionContext.Implicits.global
      val connection = driver.connection(s"localhost:$port" :: Nil)
      val db = connection("mydb")
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
          val selector = BSONDocument("pageId" -> itemIds(nb))
          Await.result(pagesCollection.update(selector, pages(nb), upsert = true), Duration(10, TimeUnit.SECONDS))
      }

      val actorRef = TestActorRef(OrderByPageLikes.props(db))
      actorRef ! CreateQuestionWithMultipleItems(userId, itemIds)

      val finishedCreation = receiveOne(Duration(10, TimeUnit.SECONDS))
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