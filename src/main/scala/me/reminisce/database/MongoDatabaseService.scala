package me.reminisce.database

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDatabaseService._
import me.reminisce.fetcher.common.GraphResponses.{Page, Post}
import me.reminisce.mongodb.MongoDBEntities._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.GetLastError

object MongoDatabaseService {
  val fbPagesCollection = "fbPages"
  val fbPageLikesCollection = "fbPageLikes"
  val fbTaggedPostsCollection = "fbTaggedPosts"
  val fbPostsCollection = "fbPosts"
  val lastFetchedCollection = "lastFetched"
  val userStatisticsCollection = "userStatistics"
  val postQuestionsCollection = "postQuestions"
  val itemsStatsCollection = "itemsStats"

  val safeLastError = new GetLastError(fsync = true)

  def props(userId: String, db: DefaultDB): Props =
    Props(new MongoDatabaseService(userId, db))

  case class SaveFBPage(pages: List[Page])

  case class SaveFBPost(posts: List[Post])

  case class SaveFBTaggedPost(posts: List[Post])

  case class SaveLastFetchedTime()

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
    FBPage(None, page.id, page.name, fbPhoto, page.likes.getOrElse(0))
  }

  def pageToFBPageLike(page: Page, userId: String): FBPageLike = {
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
    val date = formatter.parseDateTime(page.created_time)
    FBPageLike(None, userId, page.id, date)
  }

  def postToFBPost(post: Post, userId: String): FBPost = {
    val likes = post.likes.flatMap(root => root.data.map(likes => likes.map(l => FBLike(l.id, l.name))))
    val like_count = likes.map(likesList => likesList.length)
    val fbFrom = post.from.map(f => FBFrom(f.id, f.name))
    val fbComments = post.comments.flatMap(root => root.data.map(comments => comments.map { c =>
      FBComment(c.id, FBFrom(c.from.id, c.from.name), c.like_count, c.message)
    }))
    val fbCommentsCount = fbComments.map(commentsList => commentsList.length)
    val fbAttachments = post.attachments.flatMap(root => root.data.map(attachments => attachments.map {
      a =>
        val media = a.media.flatMap(m => m.image.map(image => FBMedia(image.height, image.width, image.src)))
        FBAttachment(a.description, media = media, `type` = a.`type`)
    }))
    val fbPlace: Option[FBPlace] = post.place.flatMap(place => place.name.flatMap(name => place.location.flatMap(
      location => location.latitude.flatMap(lat => location.longitude.flatMap(
        long => Some(FBPlace(place.id, name, FBLocation(location.city, location.country,
          lat, long, location.street, location.zip), place.created_time))
      ))
    )))
    FBPost(None, userId, post.id, post.message, post.story, fbPlace, post.created_time, fbFrom,
      likes, like_count, post.`type`, post.link, fbAttachments, fbComments, fbCommentsCount)
  }
}

class MongoDatabaseService(userId: String, db: DefaultDB) extends DatabaseService {

  def receive = {
    case SaveFBPage(pages) =>
      saveFBPagesToDB(pages)
    case SaveFBPost(posts) =>
      saveFBPostToDB(posts, db[BSONCollection](MongoDatabaseService.fbPostsCollection))
    case SaveFBTaggedPost(posts) =>
      saveFBPostToDB(posts, db[BSONCollection](MongoDatabaseService.fbTaggedPostsCollection))
    case SaveLastFetchedTime =>
      saveLastFetchTime(db[BSONCollection](MongoDatabaseService.lastFetchedCollection))
    case any => log.error(s"MongoDB Service received unexpected message : $any")
  }

  def saveFBPagesToDB(pages: List[Page]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val fbPageCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
    val fbPageLikeCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
    pages.foreach { p =>
      val query = BSONDocument("pageId" -> p.id)
      fbPageCollection.update(query, pageToFBPage(p), safeLastError, upsert = true)

      val query2 = BSONDocument("userId" -> userId, "pageId" -> p.id)
      fbPageLikeCollection.update(query2, pageToFBPageLike(p, userId), safeLastError, upsert = true)
    }
  }


  def saveFBPostToDB(posts: List[Post], collection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    posts.foreach { p =>
      val selector = BSONDocument("userId" -> userId, "postId" -> p.id)
      collection.update(selector, postToFBPost(p, userId), safeLastError, upsert = true)
    }
  }

  def saveLastFetchTime(collection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val time = DateTime.now
    val selector = BSONDocument("userId" -> userId)

    val update = BSONDocument("userId" -> userId, "date" -> time)

    collection.update(selector, update, safeLastError, upsert = true)
  }

}
