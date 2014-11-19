package database

import akka.actor.Props
import database.MongoDatabaseService.SaveFBPage
import messages.GraphResponses
import mongodb.MongoDBEntities.{FBPage, FBPhoto, FBTag}
import reactivemongo.api.{DefaultDB, MongoConnection}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import service.GameRequest

/**
 * Created by roger on 17/11/14.
 */

object MongoDatabaseService{
  val fbPagesCollection = "fb_pages"
  val fbPageLikesCollection = "fb_page_likes"

  def props(user_id: String, db: DefaultDB): Props =
    Props(new MongoDatabaseService(user_id, db))

  case class SaveFBPage(pages: List[GraphResponses.Page])

}

class MongoDatabaseService(user_id: String, db: DefaultDB) extends DatabaseService{

  def receive = {
    case SaveFBPage(pages) =>
      saveFBPagesToDB(pages)
    case _ => log.error(s"MongoDB Service received unexpected message")
  }

  def saveFBPagesToDB(pages: List[GraphResponses.Page]): Unit ={
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

        FBPhoto(photo.id, photo.source, photo.created_time, tags)
      }
      p.id.map{ page_id =>
        fbPageCollection.insert(FBPage(None, page_id, p.name, fbPhoto))
        fbPageLikeCollection.insert(FBPageLike(None, user_id, page_id))
      }

    }
  }
}
