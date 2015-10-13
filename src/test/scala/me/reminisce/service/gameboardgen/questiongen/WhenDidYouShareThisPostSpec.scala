package me.reminisce.service.gameboardgen.questiongen

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.github.nscala_time.time.Imports._
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.config.ConfigFactory
import me.reminisce.database.{DatabaseTestHelper, MongoDatabaseService}
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.{TextPostSubject, TimelineQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, NotEnoughData}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class WhenDidYouShareThisPostSpec extends TestKit(ActorSystem("WhenDidYouShareThisPostSpec", ConfigFactory.parseString("akka.loglevel = OFF")))
with MongoEmbedDatabase with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  var port = DatabaseTestHelper.getNewPort
  var mongoProps: MongodProps = mongoStart(port = port)
  val driver = new MongoDriver
  val connection = driver.connection(s"localhost:$port" :: Nil)
  val db = connection("mydb")

  val userId = "TestUserWhenDidYouShareThisPost"


  override def afterAll() {
    TestKit.shutdownActorSystem(system)
    db.drop()
    mongoStop(mongoProps)
    driver.system.shutdown()
    DatabaseTestHelper.releasePort(port)
  }

  "WhenDidYouShareThisPost" must {
    "not create question when there is no post." in {
      val itemId = "This post does not exist"

      val actorRef = TestActorRef(WhenDidYouShareThisPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)
      expectMsg(NotEnoughData(s"Post not found : $itemId"))
    }

    "create a valid question when the post is there." in {
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)

      val itemId = "PostId"
      val postMessage = "Awesome Message"

      val selector = BSONDocument("userId" -> userId, "pageId" -> itemId)
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
      val postedTime = DateTime.now.toString(formatter)
      val fbPost = FBPost(postId = itemId, userId = userId, attachments = None, message = Some(postMessage),
        createdTime = Some(postedTime))
      Await.result(postsCollection.update(selector, fbPost, upsert = true), Duration(10, TimeUnit.SECONDS))

      val actorRef = TestActorRef(WhenDidYouShareThisPost.props(db))
      actorRef ! CreateQuestion(userId, itemId)

      val finishedCreation = receiveOne(Duration(10, TimeUnit.SECONDS))
      assert(finishedCreation != null)
      assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

      val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
      assert(question.isInstanceOf[TimelineQuestion])

      assert(question.asInstanceOf[TimelineQuestion].subject.isDefined)
      val subject = question.asInstanceOf[TimelineQuestion].subject.get
      val answer = question.asInstanceOf[TimelineQuestion].answer

      assert(subject.isInstanceOf[TextPostSubject])
      assert(subject.asInstanceOf[TextPostSubject].text == fbPost.message.getOrElse(""))

      assert(postedTime == answer)
    }
  }

}