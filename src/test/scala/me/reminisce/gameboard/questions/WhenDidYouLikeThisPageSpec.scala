package me.reminisce.gameboard.questions

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities.{PageSubject, TimelineQuestion}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, NotEnoughData}
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class WhenDidYouLikeThisPageSpec extends QuestionTester("WhenDidYouLikeThisPageSpec") {

  val userId = "TestUserWhenDidYouLikeThisPage"

  "WhenDidYouLikeThisPage" must {
    "not create question when there is no like." in {
      testWithDb {
        db =>
          val itemId = "This page does not exist"

          val actorRef = TestActorRef(WhenDidYouLikeThisPage.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"Page or pagelike not found : user $userId, page $itemId"))
      }
    }

    "not create question when there is no page." in {
      testWithDb {
        db =>
          val itemId = "This page does not exist"

          val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
          val likedTime = DateTime.now
          val pageLike = FBPageLike(None, userId, itemId, likedTime)
          Await.result(pageLikesCollection.update(pageLike, pageLike, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhenDidYouLikeThisPage.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))
          testProbe.expectMsg(NotEnoughData(s"Page or pagelike not found : user $userId, page $itemId"))
      }
    }

    "create a valid question when the data is there." in {
      testWithDb {
        db =>
          val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)

          val itemId = s"PageId"

          val page = FBPage(None, itemId, Some(s"Cool page with id ID"), None, 1)
          Await.result(pagesCollection.update(page, page, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
          val likedTime = DateTime.now
          val pageLike = FBPageLike(None, userId, itemId, likedTime)
          Await.result(pageLikesCollection.update(pageLike, pageLike, WriteConcern.Acknowledged, upsert = true), Duration(10, TimeUnit.SECONDS))

          val actorRef = TestActorRef(WhenDidYouLikeThisPage.props(db))
          val testProbe = TestProbe()
          testProbe.send(actorRef, CreateQuestion(userId, itemId))

          checkFinished[TimelineQuestion](testProbe) {
            question =>
              checkSubject[PageSubject](question.subject) {
                subject =>
                  val answer = question.answer
                  assert(subject.name == page.name.getOrElse(""))
                  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
                  assert(likedTime.toString(formatter) == answer)
              }
          }
      }
    }
  }

}