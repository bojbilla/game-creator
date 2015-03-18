package mongodb

import com.github.nscala_time.time.Imports._
import crawler.common.GraphResponses.{From, Like, Root, Photo}
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

  case class FBPhoto(id: String, source: Option[String], created_time: Option[String], tags: Option[List[FBTag]])

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

  case class FBFrom(user_id: String, user_name:String)
  object FBFrom{
    implicit val fbFromFormat = Macros.handler[FBFrom]
  }
  case class FBLike(user_id: String, user_name: String)
  object FBLike{
    implicit val fbLikeFormat = Macros.handler[FBLike]
  }
  case class FBMedia(height: Int, width: Int, src: String)
  object FBMedia{
    implicit val fbMediaFormat = Macros.handler[FBMedia]
  }
  case class FBAttachment(description: Option[String] = None, media: Option[FBMedia] = None, `type`: Option[String] = None)
  object FBAttachment{
    implicit val fbAttachmentFormat = Macros.handler[FBAttachment]

  }
  case class FBComment(id: String, from: FBFrom, like_count: Int, message: String)
  object FBComment{
    implicit val fbCommentFormat = Macros.handler[FBComment]
  }

  case class LastCrawled(id: Option[BSONObjectID], user_id: String, date:Long)
  object LastCrawled{
    implicit val lastCrawledFormat = Macros.handler[LastCrawled]
  }

  case class FBPost(id: Option[BSONObjectID] = None,
                    user_id: String,
                    post_id: String,
                    message: Option[String] = None,
                    story: Option[String] = None,
                    created_time: Option[String] = None,
                    from: Option[FBFrom] = None,
                    likes: Option[List[FBLike]] = None,
                    like_count: Option[Int] = None,
                     `type`: Option[String] = None,
                    attachments: Option[List[FBAttachment]],
                     comments: Option[List[FBComment]] = None,
                     comments_count: Option[Int] = None)
  object FBPost{
    implicit  val fbPostFormat = Macros.handler[FBPost]
  }


}