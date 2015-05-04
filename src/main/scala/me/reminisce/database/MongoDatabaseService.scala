package me.reminisce.database

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDatabaseService._
import me.reminisce.fetcher.common.GraphResponses.{Page, Post}
import me.reminisce.mongodb.MongoDBEntities._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

object MongoDatabaseService {
  val fbPagesCollection = "fb_pages"
  val fbPageLikesCollection = "fb_page_likes"
  val fbTaggedPostsCollection = "fb_tagged_posts"
  val fbPostsCollection = "fb_posts"
  val lastFetchedCollection = "last_fetched"
  val userStatisticsCollection = "user_statistics"
  val postQuestionsCollection = "post_questions"

  def props(user_id: String, db: DefaultDB): Props =
    Props(new MongoDatabaseService(user_id, db))

  case class SaveFBPage(pages: List[Page])

  case class SaveFBPost(posts: List[Post])

  case class SaveFBTaggedPost(posts: List[Post])

  case class SaveLastFetchedTime()

}

class MongoDatabaseService(user_id: String, db: DefaultDB) extends DatabaseService {

  def receive = {
    case SaveFBPage(pages) =>
      saveFBPagesToDB(pages)
    case SaveFBPost(posts) =>
      saveFBPostToDB(posts, db[BSONCollection](MongoDatabaseService.fbPostsCollection))
    case SaveFBTaggedPost(posts) =>
      saveFBPostToDB(posts, db[BSONCollection](MongoDatabaseService.fbTaggedPostsCollection))
    case SaveLastFetchedTime =>
      saveLastFetchTime(db[BSONCollection](MongoDatabaseService.lastFetchedCollection))
    case any => log.error(s"MongoDB Service received unexpected message : " + any)
  }

  def saveFBPagesToDB(pages: List[Page]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val fbPageCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
    val fbPageLikeCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
    pages.map { p =>
      val photo = p.photos.flatMap(photoRoot => photoRoot.data.map(photo => photo))
      val fbPhoto = photo.map { photo =>
        val tags = photo.tags.flatMap(tagRoot => tagRoot.data).map {
          tags => tags.map { tag => {
            FBTag(tag.id, tag.name, tag.created_time, tag.x, tag.y)
          }
          }
        }
        FBPhoto(photo.id, photo.source, photo.created_time, tags)
      }

      val query = BSONDocument("page_id" -> p.id)
      fbPageCollection.update(query, FBPage(None, p.id, p.name, fbPhoto), upsert = true)

      val query2 = BSONDocument("user_id" -> user_id, "page_id" -> p.id)
      fbPageLikeCollection.update(query2, FBPageLike(None, user_id, p.id), upsert = true)
    }
  }


  def saveFBPostToDB(posts: List[Post], collection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    posts.foreach { p =>
      val likes = p.likes.flatMap(root => root.data.map(likes => likes.map(l => FBLike(l.id, l.name))))
      val like_count = p.likes.flatMap(root => root.summary.map(s => s.total_count))
      val fbFrom = p.from.map(f => FBFrom(f.id, f.name))
      val fbComments = p.comments.flatMap(root => root.data.map(comments => comments.map { c =>
        FBComment(c.id, FBFrom(c.from.id, c.from.name), c.like_count, c.message)
      }))
      val fbCommentsCount = p.comments.flatMap { root => root.summary.map { sum => sum.total_count } }
      val fbAttachments = p.attachments.flatMap(root => root.data.map(attachments => attachments.map {
        a =>
          val media = a.media.flatMap(m => m.image.map(image => FBMedia(image.height, image.width, image.src)))
          FBAttachment(a.description, media = media, `type` = a.`type`)
      }))
      val fbPlace: Option[FBPlace] = p.place.flatMap(place => place.name.flatMap(name => place.location.flatMap(
        location => location.latitude.flatMap(lat => location.longitude.flatMap(
          long => Some(FBPlace(place.id, name, FBLocation(location.city, location.country,
            lat, long, location.street, location.zip), place.created_time))
        ))
      )))
      val fbPost = FBPost(None, user_id, p.id, p.message, p.story, fbPlace, p.created_time, fbFrom,
        likes, like_count, p.`type`, p.link, fbAttachments, fbComments, fbCommentsCount)

      val selector = BSONDocument("user_id" -> user_id, "post_id" -> p.id)
      collection.update(selector, fbPost, upsert = true)
    }
  }

  def saveLastFetchTime(collection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val time = DateTime.now
    val selector = BSONDocument("user_id" -> user_id)

    val update = BSONDocument("user_id" -> user_id, "date" -> time)

    collection.update(selector, update, upsert = true)
  }

}
