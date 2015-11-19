package me.reminisce.database

import com.github.nscala_time.time.Imports._
import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike, FBPost}
import me.reminisce.mongodb.StatsEntities.{ItemStats, UserStats}
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
  private val collections: List[String] = List("fbPages", "fbPageLikes", "fbPosts", "userStatistics", "userStatistics")

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

  private def storeObjects(db: DefaultDB, folder: String, collectionName: String): Unit = {
    val collection = db[BSONCollection](collectionName)
    val lines = Source.fromURL(getClass.getResource(folder + "/" + collectionName + ".json")).getLines()
    save(collection, lines.map(line => getObject(collectionName, line)))
  }

  private def getObject(collectionName: String, line: String): Object = {
    val json = parse(line)
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
    collectionName match {
      case "fbPages" =>
        json.extract[FBPage]
      case "fbPageLikes" =>
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
      case "fbPosts" =>
        json.extract[FBPost]

      case "userStatistics" =>
        json.extract[UserStats]

      case "itemsStats" =>
        json.extract[ItemStats]

      case _ =>
        "Unsupported object"
    }
  }

  private def save(collection: BSONCollection, objs: Iterator[Object]): Unit = {
    objs.foreach {
      case fbPage: FBPage =>
        save[FBPage](collection, fbPage)
      case fbPageLike: FBPageLike =>
        save[FBPageLike](collection, fbPageLike)
      case fbPost: FBPost =>
        save[FBPost](collection, fbPost)
      case userStats: UserStats =>
        save[UserStats](collection, userStats)
      case itemStats: ItemStats =>
        save[ItemStats](collection, itemStats)
      case _ =>
        error("Unsupported object")
    }
  }

  private def save[T](collection: BSONCollection, obj: T): Unit = {
    val safeLastError = new GetLastError(w = Some(BSONInteger(1)))
    collection.save(obj, safeLastError)
  }

  def populateWithTestData(db: DefaultDB, folder: String): Unit = {
    collections.foreach(name => storeObjects(db, folder, name))
  }
}

