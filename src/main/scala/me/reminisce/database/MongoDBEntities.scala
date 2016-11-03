package me.reminisce.database

import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDBEntities.FBReaction
import reactivemongo.bson._

/**
  * Collections used by this project
  */
object MongoCollections {
  val fbPages = "fbPages"
  val fbPageLikes = "fbPageLikes"
  val fbPosts = "fbPosts"
  val lastFetched = "lastFetched"
  val userSummaries = "userSummaries"
  val itemsSummaries = "itemsSummaries"
}

/**
  * Defines all the facebook objects which will be stored in MongoDB and the serialization routines are defined
  */
object MongoDBEntities {

  implicit object BSONDateTimeHandler extends BSONHandler[BSONDateTime, DateTime] {
    def read(time: BSONDateTime) = new DateTime(time.value)

    def write(gdtime: DateTime) = BSONDateTime(gdtime.getMillis)
  }

  case class FBTag(id: Option[String], name: Option[String], createdTime: Option[DateTime], x: Option[Double], y: Option[Double])

  object FBTag {
    implicit val fbTagFormat = Macros.handler[FBTag]
  }

  case class FBPhoto(id: String, source: Option[String], createdTime: Option[String], tags: Option[List[FBTag]])

  object FBPhoto {
    implicit val fbPhotoFormat = Macros.handler[FBPhoto]
  }

  //the facebook page_id can't be used as a mongodb id as its too short
  case class FBPage(id: Option[BSONObjectID], pageId: String, name: Option[String], photos: Option[FBPhoto], likesNumber: Int)

  object FBPage {
    implicit val fbPageFormat = Macros.handler[FBPage]
  }

  case class FBPageLike(id: Option[BSONObjectID], userId: String, pageId: String, likeTime: DateTime)

  object FBPageLike {
    implicit val fbPageLikeFormat = Macros.handler[FBPageLike]
  }

  case class FBFrom(userId: String, userName: String)

  object FBFrom {
    implicit val fbFromFormat = Macros.handler[FBFrom]
  }

  case class FBReaction(userId: String, userName: String, reactionType: String)

  object FBReaction {
    implicit val fbReactionFormat = Macros.handler[FBReaction]
  }

  case class FBMedia(height: Int, width: Int, src: String)

  object FBMedia {
    implicit val fbMediaFormat = Macros.handler[FBMedia]
  }

  case class FBAttachment(description: Option[String] = None, media: Option[FBMedia] = None, tpe: Option[String] = None)

  object FBAttachment {
    implicit val fbAttachmentFormat = Macros.handler[FBAttachment]

  }

  case class FBComment(id: String, from: FBFrom, likeCount: Int, message: String)

  object FBComment {
    implicit val fbCommentFormat = Macros.handler[FBComment]
  }

  case class LastFetched(id: Option[BSONObjectID], userId: String, date: DateTime)

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

  case class FBPlace(id: Option[String], name: Option[String], location: FBLocation,
                     createdTime: Option[String])

  object FBPlace {
    implicit val fbPlace = Macros.handler[FBPlace]
  }


  case class FBPost(id: Option[BSONObjectID] = None,
                    userId: String,
                    postId: String,
                    message: Option[String] = None,
                    story: Option[String] = None,
                    place: Option[FBPlace] = None,
                    createdTime: Option[String] = None,
                    from: Option[FBFrom] = None,
                    reactions: Option[List[FBReaction]] = None,
                    reactionCount: Option[Int] = None,
                    tpe: Option[String] = None,
                    link: Option[String] = None,
                    picture: Option[String] = None,
                    attachments: Option[List[FBAttachment]] = None,
                    comments: Option[List[FBComment]] = None,
                    commentsCount: Option[Int] = None)


  object FBPost {
    implicit val fbPostFormat = Macros.handler[FBPost]
  }

}

/**
  * Defines all the data analysis entities which will be stored in the database along with the serialization routines
  */
object AnalysisEntities {

  case class UserSummary(id: Option[BSONObjectID] = None,
                         userId: String,
                         dataTypeCounts: Map[String, Int] = Map(),
                         questionCounts: Map[String, Int] = Map(),
                         likers: Set[FBReaction] = Set())

  object UserSummary {

    def getMapStringIntWriter(implicit intWriter: BSONWriter[Int, BSONInteger]): BSONDocumentWriter[Map[String, Int]] = {
      new BSONDocumentWriter[Map[String, Int]] {
        def write(mapIntString: Map[String, Int]): BSONDocument = {
          val elements = mapIntString.toStream.map {
            case (key, value) => key -> intWriter.write(value)
          }
          BSONDocument(elements)
        }
      }
    }

    def getMapStringIntReader(implicit intReader: BSONReader[BSONInteger, Int]): BSONDocumentReader[Map[String, Int]] = {
      new BSONDocumentReader[Map[String, Int]] {
        def read(doc: BSONDocument): Map[String, Int] = {
          val elements = doc.elements.map {
            case (key, value) => key -> intReader.read(value.seeAsOpt[BSONInteger].get)
          }
          elements.toMap
        }
      }
    }

    implicit val mapStringIntWriter = getMapStringIntWriter

    implicit val mapStringIntReader = getMapStringIntReader

    implicit val fbReactionFormat = MongoDBEntities.FBReaction.fbReactionFormat

    implicit val userSummaryFormat = Macros.handler[UserSummary]
  }

  case class PostQuestions(id: Option[BSONObjectID] = None,
                           userId: String,
                           postId: String,
                           questions: List[String],
                           questionsCount: Int)

  object PostQuestions {
    implicit val postQuestionsFormat = Macros.handler[PostQuestions]
  }


  case class ItemSummary(id: Option[BSONObjectID] = None,
                         userId: String,
                         itemId: String,
                         itemType: String,
                         dataTypes: List[String],
                         dataCount: Int,
                         readForSummary: Boolean = false)

  object ItemSummary {
    implicit val itemSummaryFormat = Macros.handler[ItemSummary]
  }

}