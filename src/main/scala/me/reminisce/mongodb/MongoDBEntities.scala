package me.reminisce.mongodb

import com.github.nscala_time.time.Imports._
import reactivemongo.bson._

object MongoDBEntities {

  implicit object BSONDateTimeHandler extends BSONHandler[BSONDateTime, DateTime] {
    def read(time: BSONDateTime) = new DateTime(time.value)

    def write(gdtime: DateTime) = BSONDateTime(gdtime.getMillis)
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

  object FBPageLike {
    implicit val fbPageLikeFormat = Macros.handler[FBPageLike]
  }

  case class FBFrom(user_id: String, user_name: String)

  object FBFrom {
    implicit val fbFromFormat = Macros.handler[FBFrom]
  }

  case class FBLike(user_id: String, user_name: String)

  object FBLike {
    implicit val fbLikeFormat = Macros.handler[FBLike]
  }

  case class FBMedia(height: Int, width: Int, src: String)

  object FBMedia {
    implicit val fbMediaFormat = Macros.handler[FBMedia]
  }

  case class FBAttachment(description: Option[String] = None, media: Option[FBMedia] = None, `type`: Option[String] = None)

  object FBAttachment {
    implicit val fbAttachmentFormat = Macros.handler[FBAttachment]

  }

  case class FBComment(id: String, from: FBFrom, like_count: Int, message: String)

  object FBComment {
    implicit val fbCommentFormat = Macros.handler[FBComment]
  }

  case class LastFetched(id: Option[BSONObjectID], user_id: String, date: DateTime)

  object LastFetched {
    implicit val lastFetchedFormat = Macros.handler[LastFetched]
  }

  case class FBLocation(city: Option[String],
                        country: Option[String],
                        latitude: Double,
                        longitude: Double,
                        street: Option[String],
                        zip: Option[String])

  object FBLocation {
    implicit val fbLocationFormat = Macros.handler[FBLocation]
  }

  case class FBPlace(id: Option[String], name: String, location: FBLocation,
                     created_time: Option[String])

  object FBPlace {
    implicit val fbPlace = Macros.handler[FBPlace]
  }


  case class FBPost(id: Option[BSONObjectID] = None,
                    user_id: String,
                    post_id: String,
                    message: Option[String] = None,
                    story: Option[String] = None,
                    place: Option[FBPlace] = None,
                    created_time: Option[String] = None,
                    from: Option[FBFrom] = None,
                    likes: Option[List[FBLike]] = None,
                    like_count: Option[Int] = None,
                    `type`: Option[String] = None,
                    attachments: Option[List[FBAttachment]],
                    comments: Option[List[FBComment]] = None,
                    comments_count: Option[Int] = None)


  object FBPost {
    implicit val fbPostFormat = Macros.handler[FBPost]
  }

  case class UserStat(id: Option[BSONObjectID] = None,
                      user_id: String,
                      question_counts: Map[String, Int] = Map(),
                      likers: Set[FBLike] = Set(),
                      max_likers_per_post: Int = 0) {

  }


  object UserStat {

    def getMapStringIntWriter(implicit intWriter: BSONWriter[Int, BSONInteger]): BSONDocumentWriter[Map[String, Int]] = {
      new BSONDocumentWriter[Map[String, Int]] {
        def write(mapIntString: Map[String, Int]): BSONDocument = {
          val elements = mapIntString.toStream.map {
            tuple => tuple._1 -> intWriter.write(tuple._2)
          }
          BSONDocument(elements)
        }
      }
    }

    def getMapStringIntReader(implicit intReader: BSONReader[BSONInteger, Int]): BSONDocumentReader[Map[String, Int]] = {
      new BSONDocumentReader[Map[String, Int]] {
        def read(doc: BSONDocument): Map[String, Int] = {
          val elements = doc.elements.map {
            tuple => tuple._1 -> intReader.read(tuple._2.seeAsOpt[BSONInteger].get)
          }
          elements.toMap
        }
      }
    }

    implicit val mapStringIntWriter = getMapStringIntWriter

    implicit val mapStringIntReader = getMapStringIntReader

    implicit val userStatFormat = Macros.handler[UserStat]
  }

  case class PostQuestions(id: Option[BSONObjectID] = None,
                           user_id: String,
                           post_id: String,
                           questions: List[String],
                           questions_count: Int)

  object PostQuestions {
    implicit val postQuestionsFormat = Macros.handler[PostQuestions]
  }

}