package me.reminisce

import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDBEntities.{FBPage, FBPageLike, FBPost}
import me.reminisce.database.StatsEntities.{ItemStats, UserStats}
import me.reminisce.database.DatabaseTestHelper._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import reactivemongo.api.DefaultDB

import scala.io.Source

object DatabaseFiller {
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
}
