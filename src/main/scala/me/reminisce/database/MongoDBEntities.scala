package me.reminisce.database

import com.github.nscala_time.time.Imports._
import me.reminisce.analysis.DataTypes._
import me.reminisce.database.MongoDBEntities.FBComment.simpleReactionFromComment
import me.reminisce.database.MongoDBEntities.{AbstractReaction, FBFriend, FBFrom}
import me.reminisce.fetching.config.GraphResponses.Friend
import me.reminisce.gameboard.board.GameboardEntities.{QuestionKind, strToKind}
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

  sealed abstract class AbstractReaction(val from: FBFrom, val reactionType: ReactionType)

  case class FBReaction(override val from: FBFrom, override val reactionType: ReactionType) extends AbstractReaction(from, reactionType)


  object AbstractReaction {
    implicit val reactionTypeWriter = new BSONWriter[ReactionType, BSONString] {
      def write(reactionType: ReactionType): BSONString = BSONString(reactionType.name)
    }

    implicit val reactionTypeReader = new BSONReader[BSONString, ReactionType] {
      def read(value: BSONString): ReactionType = stringToType(value.as[String]).asInstanceOf[ReactionType]
    }
    implicit val fbReactionFormat = Macros.handler[FBReaction]

    implicit val abstractReactionWriter = new BSONDocumentWriter[AbstractReaction] {
      override def write(reaction: AbstractReaction): BSONDocument = fbReactionFormat.write(reaction.asInstanceOf[FBReaction])
    }
    implicit val abstractReactionReader = new BSONDocumentReader[AbstractReaction] {
      override def read(bson: BSONDocument) = fbReactionFormat.read(bson)
    }
  }

  case class FBMedia(height: Int, width: Int, src: String)

  object FBMedia {
    implicit val fbMediaFormat = Macros.handler[FBMedia]
  }

  case class FBAttachment(description: Option[String] = None, media: Option[FBMedia] = None, tpe: Option[String] = None)

  object FBAttachment {
    implicit val fbAttachmentFormat = Macros.handler[FBAttachment]

  }

  case class FBComment(id: String, override val from: FBFrom, likeCount: Int, message: String) extends AbstractReaction(from, PostWhoCommented)

  object FBComment {
    implicit val fbCommentFormat = Macros.handler[FBComment]

    def simpleReactionFromComment(fBComment: FBComment): FBReaction = {
      FBReaction(fBComment.from, fBComment.reactionType)
    }
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


  // Note that here reactions and reactionCount are reactions in the way facebook sees it so posts are not included
  case class FBPost(id: Option[BSONObjectID] = None,
                    userId: String,
                    postId: String,
                    message: Option[String] = None,
                    story: Option[String] = None,
                    place: Option[FBPlace] = None,
                    createdTime: Option[String] = None,
                    from: Option[FBFrom] = None,
                    reactions: Option[Set[FBReaction]] = None,
                    reactionCount: Option[Int] = None,
                    tpe: Option[String] = None,
                    link: Option[String] = None,
                    picture: Option[String] = None,
                    attachments: Option[List[FBAttachment]] = None,
                    comments: Option[List[FBComment]] = None,
                    commentsCount: Option[Int] = None) {

    def commentsAsReactions: Set[FBReaction] = {
      comments.fold(List[FBReaction]()) {
        comments =>
          comments.map(simpleReactionFromComment)
      }.toSet
    }
  }


  object FBPost {
    implicit val fbPostFormat = Macros.handler[FBPost]
  }

  def filterReaction[React <: AbstractReaction](fbReactions: Iterable[React], reactionType: ReactionType): Iterable[React] = fbReactions.filter(
    react => react.reactionType == reactionType
  )

  // Id is impossible to get for people not playing the game
  case class FBFriend(name: String) {
    def this(friend: Friend) = this(friend.name)
  }

  object FBFriend {
    implicit val fbFriendFormat = Macros.handler[FBFriend]

    def apply(friend: Friend): FBFriend = new FBFriend(friend)
  }

}

/**
  * Defines all the data analysis entities which will be stored in the database along with the serialization routines
  */
object AnalysisEntities {

  implicit object DataTypeHandler extends BSONHandler[BSONString, DataType] {
    override def write(t: DataType): BSONString = BSONString(t.name)

    override def read(bson: BSONString): DataType = stringToType(bson.value)
  }

  case class UserSummary(id: Option[BSONObjectID] = None,
                         userId: String,
                         dataTypeCounts: Map[DataType, Int] = Map(),
                         questionCounts: Map[QuestionKind, Int] = Map(),
                         reactioners: Set[AbstractReaction] = Set(),
                         friends: Set[FBFriend] = Set(),
                         blacklist: Option[Set[FBFrom]] = None,
                         commentersCommentsCount: Map[String, Int] = Map(),
                         reactionersReactionsCount: Map[String, Int] = Map())
                         
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

    def getMapKindIntWriter(implicit intWriter: BSONWriter[Int, BSONInteger]): BSONDocumentWriter[Map[QuestionKind, Int]] = {
      new BSONDocumentWriter[Map[QuestionKind, Int]] {
        def write(mapIntString: Map[QuestionKind, Int]): BSONDocument = {
          val elements = mapIntString.toStream.map {
            case (key, value) => key.name -> intWriter.write(value)
          }
          BSONDocument(elements)
        }
      }
    }

    def getMapKindIntReader(implicit intReader: BSONReader[BSONInteger, Int]): BSONDocumentReader[Map[QuestionKind, Int]] = {
      new BSONDocumentReader[Map[QuestionKind, Int]] {
        def read(doc: BSONDocument): Map[QuestionKind, Int] = {
          val elements = doc.elements.map {
            case (key, value) => strToKind(key) -> intReader.read(value.seeAsOpt[BSONInteger].get)
          }
          elements.toMap
        }
      }
    }

    def getMapDataTypeIntWriter(implicit intWriter: BSONWriter[Int, BSONInteger]): BSONDocumentWriter[Map[DataType, Int]] = {
      new BSONDocumentWriter[Map[DataType, Int]] {
        def write(mapIntString: Map[DataType, Int]): BSONDocument = {
          val elements = mapIntString.toStream.map {
            case (key, value) => key.name -> intWriter.write(value)
          }
          BSONDocument(elements)
        }
      }
    }

    def getMapDataTypeIntReader(implicit intReader: BSONReader[BSONInteger, Int]): BSONDocumentReader[Map[DataType, Int]] = {
      new BSONDocumentReader[Map[DataType, Int]] {
        def read(doc: BSONDocument): Map[DataType, Int] = {
          val elements = doc.elements.map {
            case (key, value) => stringToType(key) -> intReader.read(value.seeAsOpt[BSONInteger].get)
          }
          elements.toMap
        }
      }
    }

    implicit val mapStringIntWriter = getMapStringIntWriter

    implicit val mapStringIntReader = getMapStringIntReader

    implicit val mapKindIntWriter = getMapKindIntWriter

    implicit val mapKindIntReader = getMapKindIntReader

    implicit val mapDataTypeIntReader = getMapDataTypeIntReader

    implicit val mapDataTypeIntWriter = getMapDataTypeIntWriter

    implicit val fbReactionFormat = MongoDBEntities.AbstractReaction.fbReactionFormat

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
                         itemType: ItemType,
                         dataTypes: Set[DataType],
                         dataCount: Int)

  object ItemSummary {
    implicit val itemTypeWriter = new BSONWriter[ItemType, BSONString] {
      def write(itemType: ItemType): BSONString = BSONString(itemType.name)
    }

    implicit val itemTypeReader = new BSONReader[BSONString, ItemType] {
      def read(value: BSONString): ItemType = strToItemType(value.as[String])
    }
    implicit val itemSummaryFormat = Macros.handler[ItemSummary]
  }

}