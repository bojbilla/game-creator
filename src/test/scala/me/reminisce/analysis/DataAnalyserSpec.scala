package me.reminisce.analysis

import java.util.concurrent.TimeUnit

import akka.testkit.TestActorRef
import com.github.nscala_time.time.Imports._
import me.reminisce.analysis.DataAnalyser.{FinalAnalysis, TransientPostsAnalysis}
import me.reminisce.analysis.DataTypes._
import me.reminisce.database.AnalysisEntities.{ItemSummary, UserSummary}
import me.reminisce.database.MongoDBEntities._
import me.reminisce.database.{MongoCollections, MongoDatabaseService}
import me.reminisce.fetching.config.GraphResponses._
import me.reminisce.gameboard.board.GameboardEntities.{Geolocation, MultipleChoice, Order, Timeline}
import me.reminisce.testutils.Retry
import me.reminisce.testutils.database.DatabaseTester
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

@DoNotDiscover
class DataAnalyserSpec extends DatabaseTester("DataAnalyserSpec") {


  "DataAnalyser" must {
    "Save \"on the fly\" analysis." in {
      testWithDb {
        db =>
          val itemsSummariesCollection = db[BSONCollection](MongoCollections.itemsSummaries)

          val ref = TestActorRef(DataAnalyser.props(AnalysisTestData.userId, db))
          ref ! TransientPostsAnalysis(AnalysisTestData.posts)

          val postIds = AnalysisTestData.posts.map(p => p.id)
          val selector = BSONDocument("userId" -> AnalysisTestData.userId, "itemId" -> BSONDocument("$in" -> postIds))

          Retry.findList[ItemSummary](itemsSummariesCollection, selector, 0) {
            _.size >= AnalysisTestData.referenceResult.size
          } match {
            case List() =>
              fail("Failed to retrieve items summaries after requesting them.")
            case summaries =>
              assert(summaries.size == AnalysisTestData.referenceResult.size)
              AnalysisTestData.referenceResult.foreach {
                itemSummary => assert(summaries.contains(itemSummary))
              }
          }
      }
    }

    "Generate new summary correctly" in {
      testWithDb {
        db =>
          val itemsSummariesCollection = db[BSONCollection](MongoCollections.itemsSummaries)
          val postCollection = db[BSONCollection](MongoCollections.fbPosts)
          val pagesCollection = db[BSONCollection](MongoCollections.fbPages)
          val pageLikesCollection = db[BSONCollection](MongoCollections.fbPageLikes)
          val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

          AnalysisTestData.referenceResult.foreach {
            itemSummary =>
              val selector = BSONDocument("userId" -> AnalysisTestData.userId, "itemId" -> itemSummary.itemId)
              Await.result(itemsSummariesCollection.update(selector, itemSummary, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          val fbPosts = AnalysisTestData.posts.map(MongoDatabaseService.postToFBPost(_, AnalysisTestData.userId))

          fbPosts.foreach {
            post =>
              val selector = BSONDocument("userId" -> AnalysisTestData.userId, "postId" -> post.postId)
              Await.result(postCollection.update(selector, post, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          AnalysisTestData.pages.foreach {
            page =>
              val selector = BSONDocument("userId" -> AnalysisTestData.userId, "pageId" -> page.pageId)
              Await.result(pagesCollection.update(selector, page, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          AnalysisTestData.pageLikes.foreach {
            pageLike =>
              val selector = BSONDocument("userId" -> AnalysisTestData.userId, "pageId" -> pageLike.pageId)
              Await.result(pageLikesCollection.update(selector, pageLike, upsert = true), Duration(10, TimeUnit.SECONDS))
          }

          val selector = BSONDocument("userId" -> AnalysisTestData.userId)

          Await.result(userSummariesCollection.update(selector, AnalysisTestData.sampleUserSummary, upsert = true), Duration(10, TimeUnit.SECONDS))

          val fbPostIds = fbPosts.map(post => post.postId).toSet
          val fbPageIds = AnalysisTestData.pages.map(page => page.pageId).toSet

          val ref = TestActorRef(DataAnalyser.props(AnalysisTestData.userId, db))
          ref ! FinalAnalysis(fbPostIds, fbPageIds)

          //likers is supposed to grow
          val userSummary = Retry.find[UserSummary](userSummariesCollection, selector, 0) {
            _.reactioners.size > AnalysisTestData.sampleUserSummary.reactioners.size
          }

          val expectedUserSummary = UserSummary(None, "TestDataAnalyserSpec",
            Map(LikeNumber -> 22, PostWhoReacted -> 1, PostWhoCommented -> 2, PostGeolocation -> 2, Time -> 22,
              PostCommentsNumber -> 2), Map(Order -> 42, MultipleChoice -> 3, Geolocation -> 2, Timeline -> 22),
            Set(FBReaction("1", "me", ""), FBReaction("2", "me2", ""), FBReaction("3", "me3", ""), FBReaction("4", "me4", "")))

          assert(userSummary.contains(expectedUserSummary))
      }
    }
  }
}

object AnalysisTestData {

  val userId = "TestDataAnalyserSpec"

  val like1 = Reaction("1", "me", "")
  val likes1 = Root[List[Reaction]](data = Some(List(like1)), paging = None, summary = None)
  val pLikes = Post("id1", from = None, message = Some("Message"), story = Some("Story"), place = None, reactions = Some(likes1),
    `type` = None, link = None, created_time = None, attachments = None, comments = None)

  val from2 = From(id = "2", name = "a")
  val comment2 = Comment(id = "", from = from2, like_count = 0, message = "", attachments = None)
  val from3 = From(id = "3", name = "s")
  val comment3 = Comment(id = "3", from = from3, like_count = 0, message = "", attachments = None)
  val from4 = From(id = "4", name = "d")
  val comment4 = Comment(id = "4", from = from4, like_count = 0, message = "", attachments = None)
  val from5 = From(id = "5", name = "f")
  val comment5 = Comment(id = "5", from = from5, like_count = 0, message = "", attachments = None)
  val com3 = Root[List[Comment]](data = Option(List(comment2, comment3, comment4, comment5)), paging = None, summary = None)
  val pComments = Post("id2", from = None, message = Some("Message"), story = Some("Story"), place = None, reactions = None,
    `type` = None, link = None, created_time = None, attachments = None, comments = Some(com3))

  val l4 = Location(city = None, country = None, latitude = Some(1.0), longitude = Some(1.2), street = None,
    zip = None)
  val pl5 = Place(id = None, name = None, location = Some(l4), created_time = None)
  val pLoc = Post("id3", from = None, message = None, story = None, place = Some(pl5),
    reactions = None, `type` = None, link = None, created_time = None, attachments = None, comments = None)

  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
  val now = DateTime.now
  val nowString = now.toString(formatter)
  val pTime = Post("id4", from = None, message = Some("message"), story = None, place = None, reactions = None,
    `type` = None, link = None, created_time = Some(nowString), attachments = None, comments = None)

  val posts = List(pLikes, pComments, pLoc, pTime)

  val referenceResult =
    List(ItemSummary(None, "TestDataAnalyserSpec", "id1", "Post", List(LikeNumber), 1),
      ItemSummary(None, "TestDataAnalyserSpec", "id2", "Post", List(PostWhoCommented, PostCommentsNumber), 2),
      ItemSummary(None, "TestDataAnalyserSpec", "id3", "Post", List(PostGeolocation), 1),
      ItemSummary(None, "TestDataAnalyserSpec", "id4", "Post", List(Time), 1))

  val pages = (1 to 10).map {
    index =>
      FBPage(None, s"${index}I$index", Some(s"Page$index"), None, index * index * 7)
  }

  val pageLikes = (1 to 5).map {
    index =>
      FBPageLike(None, s"$userId", pages(index).pageId, now - index.months)
  }

  val sampleUserSummary = UserSummary(None, userId,
    Map(LikeNumber -> 11, PostWhoCommented -> 1, PostGeolocation -> 1, Time -> 11, PostCommentsNumber -> 1),
    Map(Order -> 18, MultipleChoice -> 1, Geolocation -> 1, Timeline -> 11),
    Set(FBReaction("2", "me2", ""), FBReaction("3", "me3", ""), FBReaction("4", "me4", "")))

}