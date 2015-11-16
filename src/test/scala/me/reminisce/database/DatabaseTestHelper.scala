package me.reminisce.database

import com.github.nscala_time.time.Imports._
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike, FBPost}
import me.reminisce.mongodb.StatsEntities.{ItemStats, UserStats}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}
import reactivemongo.bson.{BSONInteger, BSONObjectID}
import reactivemongo.core.commands.GetLastError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

object DatabaseTestHelper extends MongoEmbedDatabase {

  case class FBPageLikeWithoutDate(id: Option[BSONObjectID], userId: String, pageId: String, likeTime: String)

  // does not conflict with live mongo instances
  private val port = 28000
  private val mongoProps: MongodProps = mongoStart(port = port)
  private lazy val driver: MongoDriver = new MongoDriver
  private lazy val connection: MongoConnection = driver.connection(s"localhost:$port" :: Nil)

  def closeConnection() = {
    this.synchronized {
      mongoStop(mongoProps)
      driver.system.shutdown()
    }
  }

  def getConnection: MongoConnection = {
    this.synchronized {
      connection
    }
  }

  def populateWithTestData(db: DefaultDB, folder: String): Unit = {
    implicit val formats = DefaultFormats
    val safeLastError = new GetLastError(w = Some(BSONInteger(1)))
    val pagesLines = Source.fromURL(getClass.getResource(folder + "/fbPages.json")).getLines()
    val pages = pagesLines.map {
      l =>
        val json = parse(l)
        json.extract[FBPage]
    }


    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)

    val fbPageCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
    pages.foreach {
      p =>
        fbPageCollection.save(p, safeLastError)
    }


    val pageLikesLines = Source.fromURL(getClass.getResource(folder + "/fbPageLikes.json")).getLines()
    val pageLikes = pageLikesLines.map {
      l =>
        val json = parse(l)
        // this allows the extract method to work properly
        val converted = json.mapField {
          case (field, value) =>
            if (field == "likeTime") {
              value.children.headOption match {
                case Some(str) =>
                  (field, str)
                case None =>
                  (field, value)
              }
            } else {
              (field, value)
            }
        }
        converted.extract[FBPageLikeWithoutDate] match {
          case FBPageLikeWithoutDate(id, userId, pageId, likeTime) =>
            FBPageLike(id, userId, pageId, formatter.parseDateTime(likeTime))
        }
    }

    val fbPageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
    pageLikes.foreach {
      pl =>
        fbPageLikesCollection.save(pl, safeLastError)
    }

    val postsLines = Source.fromURL(getClass.getResource(folder + "/fbPosts.json")).getLines()
    val posts = postsLines.map {
      l =>
        val json = parse(l)
        json.extract[FBPost]
    }

    val fbPostsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
    posts.foreach {
      p =>
        fbPostsCollection.save(p, safeLastError)
    }

    val userStatsLines = Source.fromURL(getClass.getResource(folder + "/userStatistics.json")).getLines()
    val userStats = userStatsLines.map {
      l =>
        val json = parse(l)
        json.extract[UserStats]
    }.toList


    val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    userStats.foreach {
      u =>
        userStatsCollection.save(u, safeLastError)
    }

    val itemStatsLines = Source.fromURL(getClass.getResource(folder + "/itemsStats.json")).getLines()
    val itemsStats = itemStatsLines.map {
      l =>
        val json = parse(l)
        json.extract[ItemStats]
    }

    val itemsStatsCollection = db[BSONCollection](MongoDatabaseService.itemsStatsCollection)
    itemsStats.foreach {
      is =>
        itemsStatsCollection.save(is, safeLastError)
    }
  }
}

