package me.reminisce

import com.github.nscala_time.time.Imports._
import me.reminisce.analysis.DataTypes.{DataType, stringToType}
import me.reminisce.database.AnalysisEntities.{ItemSummary, UserSummary}
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.{FBPage, FBPageLike, FBPost, FBReaction}
import me.reminisce.gameboard.board.GameboardEntities.strToKind
import me.reminisce.testutils.database.DatabaseTestHelper._
import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods._
import org.json4s.{CustomSerializer, DefaultFormats}
import reactivemongo.api.DefaultDB

import scala.io.Source

object DatabaseFiller {

  class ItemSummarySerializer extends CustomSerializer[ItemSummary](implicit formats => ( {
    case summary: JObject =>
      val id = None
      val userId = (summary \ "userId").extract[String]
      val itemId = (summary \ "itemId").extract[String]
      val itemType = (summary \ "itemType").extract[String]
      val dataTypes = (summary \ "dataTypes").extract[Array[Any]].map(x => stringToType(x.asInstanceOf[String])).toList
      val dataCount = (summary \ "dataCount").extract[Int]
      ItemSummary(id, userId, itemId, itemType, dataTypes, dataCount)
  }, {
    case ItemSummary(id, userId, itemId, itemType, dataTypes, dataCount) =>
      JObject(List(JField("userId", JString(userId)), JField("itemId", JString(itemId)), JField("itemType", JString(itemType)),
        JField("dataTypes", JArray(dataTypes.map(dType => JString(dType.name)))), JField("dataCount", JInt(dataCount))))
  }))

  def populateWithTestData(db: DefaultDB, folder: String): Unit = {

    implicit val formats = DefaultFormats + new CustomSerializer[DataType](implicit formats => ( {
      case dType: JString =>
        stringToType(dType.values)
    }, {
      case dType: DataType =>
        JString(dType.name)
    })) + new CustomSerializer[List[DataType]](implicit formats => ( {
      case dTypes: JArray =>
        dTypes.arr.map(x => x.extract[DataType])
    }, {
      case dTypes: List[DataType] =>
        JArray(dTypes.map(x => JString(x.name)))
    }))

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
    storeObjects(db, MongoCollections.fbPageLikes, pageLikes)

    val posts = simpleExtract[FBPost](folder + "/fbPosts.json")
    storeObjects(db, MongoCollections.fbPosts, posts)
    val userSummariesLines = Source.fromURL(getClass.getResource(folder + "/userSummaries.json")).getLines()
    val userSummaries = userSummariesLines.map {
      l =>
        val json = parse(l)

        json match {
          case summary: JObject =>
            val id = None
            val userId = (summary \ "userId").extract[String]
            val dataTypeCounts = (summary \ "dataTypeCounts").extract[Map[String, Int]].map {
              case (key, value) => stringToType(key) -> value
            }
            val questionCounts = (summary \ "questionCounts").extract[Map[String, Int]].map {
              case (key, value) => strToKind(key) -> value
            }
            val reactioners = (summary \ "reactioners").extract[Set[FBReaction]]

            Some(UserSummary(id, userId, dataTypeCounts, questionCounts, reactioners))
          case _ =>
            None
        }
    }.flatten
    storeObjects(db, MongoCollections.userSummaries, userSummaries)

    val itemsSummariesLines = Source.fromURL(getClass.getResource(folder + "/itemsSummaries.json")).getLines()
    val itemsSummaries = itemsSummariesLines.map {
      l =>
        val json = parse(l)

        json match {
          case summary: JObject =>
            val id = None
            val userId = (summary \ "userId").extract[String]
            val itemId = (summary \ "itemId").extract[String]
            val itemType = (summary \ "itemType").extract[String]
            val dataTypes = (summary \ "dataTypes").extract[List[DataType]]
            val dataCount = (summary \ "dataCount").extract[Int]
            Some(ItemSummary(id, userId, itemId, itemType, dataTypes, dataCount))
          case _ =>
            None
        }
    }.flatten
    storeObjects(db, MongoCollections.itemsSummaries, itemsSummaries)
  }
}
