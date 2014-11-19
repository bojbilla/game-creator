package mongodb

import com.github.nscala_time.time.Imports._
import messages.GraphResponses.{Root, Photo}
import org.joda.time.DateTime
import reactivemongo.bson.Macros.Options.{\/, UnionType, AllImplementations}
import reactivemongo.bson._
/**
 * Created by roger on 15/11/14.
 */

object MongoDBEntities {

  implicit object BSONDateTimeHandler extends BSONHandler[BSONDateTime, DateTime] {
    def read(time: BSONDateTime) = new DateTime(time.value)

    def write(jdtime: DateTime) = BSONDateTime(jdtime.getMillis)
  }

  case class FBTag(id: Option[String], name: Option[String], created_time: Option[DateTime], x: Option[Double], y: Option[Double])

  object FBTag {
    implicit val fbTagFormat = Macros.handler[FBTag]
  }

  case class FBPhoto(id: String, source: Option[String], created_time: Option[DateTime], tags: Option[List[FBTag]])

  object FBPhoto {
    implicit val fbPhotoFormat = Macros.handler[FBPhoto]
  }
  //the facebook page_id can't be used as a mongodb id as its too short
  case class FBPage(id: Option[BSONObjectID], page_id: String, name: Option[String], photos: Option[FBPhoto]) {
  }
  object FBPage {
    implicit val fbPageFormat = Macros.handler[FBPage]
  }
  case class FBPageLike(id: Option[BSONObjectID], user_id: String, page_id: String)
  object FBPageLike{
    implicit val fbPageLikeFormat = Macros.handler[FBPageLike]
  }


}