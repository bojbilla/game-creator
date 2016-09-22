package me.reminisce.database

import com.github.nscala_time.time.Imports._
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import me.reminisce.database.MongoDBEntities.{FBPage, FBPageLike, FBPost}
import me.reminisce.database.StatsEntities.{ItemStats, UserStats}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}
import reactivemongo.bson.{BSONDocumentWriter, BSONObjectID}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

object DatabaseTestHelper extends MongoEmbedDatabase {

  private lazy val driver: MongoDriver = new MongoDriver
  private lazy val connection: MongoConnection = driver.connection(s"localhost:$port" :: Nil)
  // this conflicts with live mongo instances
  val port = 27017
  private val mongoProps: MongodProps = mongoStart(port = port)

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

    val pages = simpleExtract[FBPage](folder + "/fbPages.json")
    storeObjects(db, "fbPages", pages)
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
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
          case _ =>
            throw new IllegalArgumentException("Impossible match case.")
        }
    }
    storeObjects(db, "fbPageLikes", pageLikes)

    val posts = simpleExtract[FBPost](folder + "/fbPosts.json")
    storeObjects(db, "fbPosts", posts)
    val userStats = simpleExtract[UserStats](folder + "/userStatistics.json")
    storeObjects(db, "userStatistics", userStats)
    val itemsStats = simpleExtract[ItemStats](folder + "/itemsStats.json")
    storeObjects(db, "itemsStats", itemsStats)
  }

  private def storeObjects[T](db: DefaultDB, collectionName: String, objects: Iterator[T])(implicit writer: BSONDocumentWriter[T]): Unit = {
    val collection = db[BSONCollection](collectionName)
    save[T](collection, objects)
  }

  private def save[T](collection: BSONCollection, objs: Iterator[T])(implicit writer: BSONDocumentWriter[T]): Unit = {
    objs.foreach {
      obj =>
        collection.update(obj, obj, WriteConcern.Acknowledged, upsert = true)
    }
  }

  private def simpleExtract[T](jsonPath: String)(implicit manifest: Manifest[T]): Iterator[T] = {
    implicit val formats = DefaultFormats
    val lines = Source.fromURL(getClass.getResource(jsonPath)).getLines()
    lines.map {
      l =>
        val json = parse(l)
        json.extract[T]
    }
  }

  case class FBPageLikeWithoutDate(id: Option[BSONObjectID], userId: String, pageId: String, likeTime: String)
}
