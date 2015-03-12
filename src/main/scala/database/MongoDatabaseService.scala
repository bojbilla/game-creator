package database

import akka.actor.Props
import database.MongoDatabaseService.{SaveFBTaggedPost, SaveFBPost, SaveFBPage}
import mongodb.MongoDBEntities._
import reactivemongo.api.{DefaultDB, MongoConnection}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import reactivemongo.core.commands.{Count, FindAndModify}
import service.GameRequest
import com.github.nscala_time.time.Imports._
import crawler.common.GraphResponses._


/**
 * Created by roger on 17/11/14.
 */

object MongoDatabaseService{
  val fbPagesCollection = "fb_pages"
  val fbPageLikesCollection = "fb_page_likes"
  val fbTaggedPostsCollection = "fb_tagged_posts"
  val fbPostsCollection = "fb_posts"

  def props(user_id: String, db: DefaultDB): Props =
    Props(new MongoDatabaseService(user_id, db))

  case class SaveFBPage(pages: List[Page])
  case class SaveFBPost(posts: List[Post])
  case class SaveFBTaggedPost(posts: List[Post])
}

class MongoDatabaseService(user_id: String, db: DefaultDB) extends DatabaseService{

  def receive = {
    case SaveFBPage(pages) =>
      saveFBPagesToDB(pages)
    case SaveFBPost(posts) =>
      saveFBPostToDB(posts, db[BSONCollection](MongoDatabaseService.fbPostsCollection))
    case SaveFBTaggedPost(posts) =>
      saveFBPostToDB(posts, db[BSONCollection](MongoDatabaseService.fbTaggedPostsCollection))
    case _ => log.error(s"MongoDB Service received unexpected message")
  }

  def saveFBPagesToDB(pages: List[Page]): Unit ={
    import reactivemongo.api._
    import scala.concurrent.ExecutionContext.Implicits.global
    import mongodb.MongoDBEntities._
    val fbPageCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
    val fbPageLikeCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
    val fbPages = pages.map { p =>
      val photo = p.photos.flatMap(photoRoot => photoRoot.data.map(photo => photo))
      val fbPhoto = photo.map{photo =>
        val tags = photo.tags.flatMap(tagRoot => tagRoot.data).map {
          tags => tags.map { tag => {
            FBTag(tag.id, tag.name, tag.created_time, tag.x, tag.y)
          }
          }
        }
//        val createDate = photo.created_time.map(t => new DateTime(t.toLong * 1000))
        FBPhoto(photo.id, photo.source, photo.created_time, tags)
      }

      val query = BSONDocument("page_id" -> p.id)
      val futureCount = db.command(Count(fbPageCollection.name, Some(query)))
      futureCount.map{ count =>
        if (count < 1){
          fbPageCollection.insert(FBPage(None, p.id, p.name, fbPhoto))
        }
      }

      val query2 = BSONDocument( "user_id" -> user_id, "page_id" -> p.id)
      val futureRelationCount = db.command(Count(fbPageLikeCollection.name, Some(query2)))
      futureRelationCount.map{ count =>
        if (count < 1){
          fbPageLikeCollection.insert(FBPageLike(None, user_id, p.id))
        }
      }
    }
  }


  def saveFBPostToDB(posts: List[Post], collection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import mongodb.MongoDBEntities._
    val fbPosts = posts.foreach{ p =>
      val likes = p.likes.map(root => root.data.map(likes => likes.map(l=> FBLike(l.id, l.name)))).flatten
      val like_count = p.likes.map(root => root.summary.map(s => s.total_count)).flatten
      val fbFrom = p.from.map(f => FBFrom(f.id, f.name))
      val fbComments = p.comments.flatMap(root => root.data.map(comments => comments.map{c =>
        FBComment(c.id, FBFrom(c.from.id, c.from.name), c.like_count, c.message)
      } ))
      val fbCommentsCount = p.comments.flatMap{ root => root.summary.map{sum => sum.total_count}}
      val fbAttachments = p.attachments.flatMap(root => root.data.map( attachments => attachments.map{
      a =>
        val media = a.media.flatMap(m => m.image.map(image => FBMedia(image.height, image.width, image.src)))
        FBAttachment(a.description, media = media, `type` = a.`type`)
    }))
      val fbPost = FBPost(None, user_id, p.id, p.message, p.story, p.created_time, fbFrom, likes, like_count, p.`type`, fbAttachments, fbComments, fbCommentsCount)
      collection.insert(fbPost)
    }
  }
}
