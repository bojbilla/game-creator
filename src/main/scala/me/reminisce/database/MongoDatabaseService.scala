package me.reminisce.database

import akka.actor.{Actor, ActorLogging, Props}
import com.github.nscala_time.time.Imports._
import me.reminisce.analysis.DataTypes._
import me.reminisce.database.MongoDBEntities._
import me.reminisce.database.MongoDatabaseService._
import me.reminisce.fetching.config.GraphResponses.{Page, Post}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Factory for [[me.reminisce.database.MongoDatabaseService]], collection names definition, case class for message
  * passing and data conversion methods
  */
object MongoDatabaseService {
  /**
    * Creates a database service actor
    *
    * @param userId userId of the user fdor which the data is stored
    * @param db     database into which data is inserted
    * @return props for the created MongoDatabaseService
    */
  def props(userId: String, db: DefaultDB): Props =
  Props(new MongoDatabaseService(userId, db))

  /**
    * Converts a [[me.reminisce.fetching.config.GraphResponses.Page]] to a [[me.reminisce.database.MongoDBEntities.FBPage]]
    *
    * @param page page to convert
    * @return FBPage resulting from the conversion
    */
  def pageToFBPage(page: Page): FBPage = {
    val photo = page.photos.flatMap(photoRoot => photoRoot.data.map(photo => photo))
    val fbPhoto = photo.map { photo =>
      val tags = photo.tags.flatMap(tagRoot => tagRoot.data).map {
        tags => tags.map { tag => {
          FBTag(tag.id, tag.name, tag.created_time, tag.x, tag.y)
        }
        }
      }
      FBPhoto(photo.id, photo.source, photo.created_time, tags)
    }
    FBPage(None, page.id, page.name, fbPhoto, page.fan_count.getOrElse(0))
  }

  /**
    * Extracts a [[me.reminisce.database.MongoDBEntities.FBPageLike]] from a
    * [[me.reminisce.fetching.config.GraphResponses.Page]]
    *
    * @param page   page from which information is extracted
    * @param userId id of the user who liked the page
    * @return the extracted FBPageLike
    */
  def pageToFBPageLike(page: Page, userId: String): FBPageLike = {
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
    val date = formatter.parseDateTime(page.created_time)
    FBPageLike(None, userId, page.id, date)
  }

  /**
    * Converts a [[me.reminisce.fetching.config.GraphResponses.Post]] to a [[me.reminisce.database.MongoDBEntities.FBPost]]
    *
    * @param post   post to convert
    * @param userId user who made the post
    * @return FBPost resulting from the conversion
    */
  def postToFBPost(post: Post, userId: String): FBPost = {
    val reactions = post.reactions.flatMap {
      root =>
        root.data.map {
          reactions => reactions.map {
            r => FBReaction(FBFrom(r.id, r.name), stringTypeToReactionType(r.`type`))
          }.toSet
        }
    }
    val reactionCount = reactions.map(reactionsList => reactionsList.size)
    val fbFrom = post.from.map(f => FBFrom(f.id, f.name))
    val fbComments = post.comments.flatMap(root => root.data.map(comments => comments.map { c =>
      FBComment(c.id, FBFrom(c.from.id, c.from.name), c.like_count, c.message)
    }))
    val fbCommentsCount = fbComments.map(commentsList => commentsList.length)
    val fbAttachments = post.attachments.flatMap(root => root.data.map(attachments => attachments.map {
      a =>
        val media = a.media.flatMap(m => m.image.map(image => FBMedia(image.height, image.width, image.src)))
        FBAttachment(a.description, media = media, tpe = a.`type`)
    }))
    val fbPlace: Option[FBPlace] = post.place.flatMap(place => place.location.flatMap(
      location => location.latitude.flatMap(lat => location.longitude.flatMap(
        long => Some(FBPlace(place.id, place.name, FBLocation(location.city, location.country,
          lat, long, location.street, location.zip), place.created_time))
      ))
    ))
    FBPost(None, userId, post.id, post.message, post.story, fbPlace, post.created_time, fbFrom,
      reactions, reactionCount, post.`type`, post.link, post.full_picture, fbAttachments, fbComments, fbCommentsCount)
  }

  case class SaveFBPage(pages: List[Page])

  case class SaveFBPost(posts: List[Post])

  case object SaveLastFetchedTime

  val stringTypeToReactionType = Map[String, ReactionType](
    "LIKE" -> PostWhoLiked,
    "WOW" -> PostWhoWowed,
    "HAHA" -> PostWhoLaughed,
    "LOVE" -> PostWhoLoved,
    "SAD" -> PostWhoGotSad,
    "ANGRY" -> PostWhoGotAngry
  )

}

class MongoDatabaseService(userId: String, db: DefaultDB) extends Actor with ActorLogging {

  /**
    * Entry point of the service, handles the following messages:
    * - SaveFBPage(pages): save the pages
    * - SaveFBPost(posts): save the posts
    * - SaveLastFetchedTime: save current time as last fetched time
    *
    * @return Nothing
    */
  def receive = {
    case SaveFBPage(pages) =>
      saveFBPagesToDB(pages)
    case SaveFBPost(posts) =>
      saveFBPostToDB(posts, db[BSONCollection](MongoCollections.fbPosts))
    case SaveLastFetchedTime =>
      saveLastFetchTime(db[BSONCollection](MongoCollections.lastFetched))
    case any => log.error(s"MongoDB Service received unexpected message : $any")
  }

  /**
    * Converts and saves pages, extracts and saves the page likes
    *
    * @param pages pages to work on
    */
  private def saveFBPagesToDB(pages: List[Page]): Unit = {
    val fbPageCollection = db[BSONCollection](MongoCollections.fbPages)
    val fbPageLikeCollection = db[BSONCollection](MongoCollections.fbPageLikes)
    pages.foreach { p =>
      val query = BSONDocument("pageId" -> p.id)
      fbPageCollection.update(query, pageToFBPage(p), WriteConcern.Acknowledged, upsert = true).onFailure {
        case e => log.error(s"TEST DEBUG PRINT : finished with error : $e")
      }

      val query2 = BSONDocument("userId" -> userId, "pageId" -> p.id)
      fbPageLikeCollection.update(query2, pageToFBPageLike(p, userId), WriteConcern.Acknowledged, upsert = true).onFailure {
        case e => log.error(s"TEST DEBUG PRINT : finished with error : $e")
      }
    }
  }

  /**
    * Converts and saves posts
    *
    * @param posts      posts to work on
    * @param collection collection in which the posts are saved
    */
  private def saveFBPostToDB(posts: List[Post], collection: BSONCollection): Unit = {
    posts.foreach { p =>
      val selector = BSONDocument("userId" -> userId, "postId" -> p.id)
      collection.update(selector, postToFBPost(p, userId), WriteConcern.Acknowledged, upsert = true).onFailure {
        case e => log.error(s"TEST DEBUG PRINT : finished with error : $e")
      }
    }
  }

  /**
    * Saves the current time as last fetched time
    *
    * @param collection collection in which the time is stored
    */
  private def saveLastFetchTime(collection: BSONCollection): Unit = {
    val time = DateTime.now
    val selector = BSONDocument("userId" -> userId)

    val update = BSONDocument("userId" -> userId, "date" -> time)

    collection.update(selector, update, WriteConcern.Acknowledged, upsert = true).onFailure {
      case e => log.error(s"TEST DEBUG PRINT : finished with error : $e")
    }
  }

}
