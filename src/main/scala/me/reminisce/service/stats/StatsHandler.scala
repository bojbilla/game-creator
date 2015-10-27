package me.reminisce.service.stats

import akka.actor.Props
import me.reminisce.database.{DatabaseService, MongoDatabaseService}
import me.reminisce.fetcher.common.GraphResponses.Post
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind.Order
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerationConfig
import me.reminisce.service.stats.StatsDataTypes.{PostGeolocation, _}
import me.reminisce.service.stats.StatsHandler.{FinalStats, TransientPostsStats, _}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.Count

import scala.annotation.tailrec
import scala.util.{Failure, Success}

object StatsHandler {

  case class FinalStats(fbPosts: Set[String], fbPages: Set[String])

  case class TransientPostsStats(fbPosts: List[Post])

  def props(userId: String, db: DefaultDB): Props =
    Props(new StatsHandler(userId, db))

  private def addTypeToMap(typeAndCount: (String, Int), oldMap: Map[String, Int]): Map[String, Int] = {
    if (oldMap.contains(typeAndCount._1)) {
      oldMap.updated(typeAndCount._1, oldMap(typeAndCount._1) + typeAndCount._2)
    } else {
      oldMap.updated(typeAndCount._1, typeAndCount._2)
    }
  }

  @tailrec
  def addTypesToMap(typesAndCounts: List[(String, Int)], oldMap: Map[String, Int]): Map[String, Int] = {
    typesAndCounts match {
      case Nil => oldMap
      case x :: xs => addTypesToMap(xs, addTypeToMap(x, oldMap))
    }
  }

  // This method is meant to be used to compute transient stats on posts
  def availableDataTypes(post: Post): List[DataType] = {
    List(hasTimeData(post), hasGeolocationData(post), hasWhoCommentedData(post),
      hasCommentNumber(post), hasLikeNumber(post)).flatten
  }

  private def hasTimeData(post: Post): Option[DataType] = {
    if ((post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty)) && post.created_time.nonEmpty)
      Some(Time)
    else
      None
  }

  private def hasGeolocationData(post: Post): Option[DataType] = {
    post.place.flatMap(place => place.location.flatMap(
      location => location.latitude.flatMap(lat => location.longitude.map(
        long => PostGeolocation
      ))
    ))
  }

  private def hasWhoCommentedData(post: Post): Option[DataType] = {
    val fbComments = post.comments.flatMap(root => root.data.map(comments => comments.map { c =>
      FBComment(c.id, FBFrom(c.from.id, c.from.name), c.like_count, c.message)
    }))
    if ((post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty)) && fbComments.nonEmpty) {
      val fromSet = fbComments.get.map {
        comm => comm.from
      }.toSet
      if (fromSet.size > 3) {
        Some(PostWhoCommented)
      } else {
        None
      }
    } else {
      None
    }
  }

  private def hasCommentNumber(post: Post): Option[DataType] = {
    val fbComments = post.comments.flatMap(root => root.data.map(comments => comments.map { c =>
      FBComment(c.id, FBFrom(c.from.id, c.from.name), c.like_count, c.message)
    }))
    if ((post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty)) && fbComments.nonEmpty) {
      if (fbComments.get.size > 3) {
        Some(PostCommentsNumber)
      } else {
        None
      }
    } else {
      None
    }
  }

  private def hasLikeNumber(post: Post): Option[DataType] = {
    val likeCount = post.likes.flatMap(root => root.data).getOrElse(List()).size
    if (likeCount > 0) {
      Some(LikeNumber)
    } else {
      None
    }
  }

  def getItemStats(userId: String, itemId: String, itemType: String, itemsStats: List[ItemStats]): ItemStats = {
    itemsStats.filter(is => is.userId == userId && is.itemId == itemId) match {
      case Nil =>
        ItemStats(None, userId, itemId, itemType, List(), 0)
      case head :: tail =>
        //Normally only one match is possible
        head
    }
  }

  def userStatsWithNewCounts(newLikers: Set[FBLike], newItemsStats: List[ItemStats], userStats: UserStats): UserStats = {
    val newDataTypes = newItemsStats.foldLeft(userStats.dataTypeCounts) {
      case (acc, itemStats) => addTypesToMap(itemStats.dataTypes.map(dType => (dType, 1)), acc)
    }

    // One has to be careful as the count for order is just the count of items that have a data type suited for ordering
    // Ordering have to be a multiple of the number of items to order
    val newQuestionCounts: Map[String, Int] = newDataTypes.foldLeft(Map[String, Int]()) {
      case (acc, cpl) =>
        val kinds = possibleKind(stringToType(cpl._1))
        val newCounts = kinds.map {
          kind =>
            val count = kind match {
              case Order =>
                val excess = cpl._2 % QuestionGenerationConfig.orderingItemsNumber
                cpl._2 - excess
              case _ =>
                cpl._2
            }
            (kind.toString, count)
        }
        addTypesToMap(newCounts, acc)
    }

    UserStats(userStats.id, userStats.userId, newDataTypes, newQuestionCounts, newLikers)
  }

}

class StatsHandler(userId: String, db: DefaultDB) extends DatabaseService {

  def receive = {
    case FinalStats(fbPosts, fbPages) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      val userStatsCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)
      val itemsStatsCollection = db[BSONCollection](MongoDatabaseService.itemsStatsCollection)
      val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
      val selector = BSONDocument("userId" -> userId)
      userStatsCollection.find(selector).one[UserStats].onComplete {
        case Success(userStatsOpt) =>
          lazy val emptyUserStats = UserStats(None, userId, Map(), Map(), Set())
          finalizeStats(fbPosts.toList, fbPages.toList, userStatsOpt.getOrElse(emptyUserStats), postCollection,
            pagesCollection, userStatsCollection, itemsStatsCollection)
        case Failure(e) =>
          log.error(s"Database could not be reached : $e.")
      }
    case TransientPostsStats(fbPosts) =>
      saveTransientPostsStats(fbPosts, db[BSONCollection](MongoDatabaseService.itemsStatsCollection))
    case any =>
      log.error("StatsHandler received unhandled message : " + any)
  }


  def saveTransientPostsStats(fbPosts: List[Post], itemsStatsCollection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    fbPosts.foreach {
      post =>
        val selector = BSONDocument("userId" -> userId, "itemId" -> post.id)
        val availableData = availableDataTypes(post).map(dType => dType.name)
        val itemStats = ItemStats(None, userId, post.id, "Post", availableData, availableData.length)
        itemsStatsCollection.update(selector, itemStats, upsert = true)
    }

  }

  def finalizeStats(fbPostsIds: List[String], fbPagesIds: List[String], userStats: UserStats,
                    postCollection: BSONCollection, pagesCollection: BSONCollection, userStatsCollection: BSONCollection,
                    itemsStatsCollection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    if ((fbPagesIds ++ fbPostsIds).length > 0) {
      val postSelector = BSONDocument("userId" -> userId, "postId" -> BSONDocument("$in" -> fbPostsIds))
      val postsCursor = postCollection.find(postSelector).cursor[FBPost]
      postsCursor.collect[List](fbPostsIds.length, stopOnError = true).onComplete {
        case Success(fbPosts: List[FBPost]) =>
          val pageSelector = BSONDocument("pageId" -> BSONDocument("$in" -> fbPagesIds))
          val pagesCursor = pagesCollection.find(pageSelector).cursor[FBPage]
          pagesCursor.collect[List](fbPagesIds.length, stopOnError = true).onComplete {
            case Success(fbPages: List[FBPage]) =>
              val itemStatsSelector = BSONDocument("userId" -> userId, "itemId" -> BSONDocument("$in" -> (fbPostsIds ++ fbPagesIds)))
              val itemsStatsCursor = itemsStatsCollection.find(itemStatsSelector).cursor[ItemStats]
              itemsStatsCursor.collect[List](fbPagesIds.length + fbPostsIds.length, stopOnError = true).onComplete {
                case Success(itemsStats: List[ItemStats]) =>
                  val queryNotLiked = BSONDocument("userId" -> userId, "pageId" -> BSONDocument("$nin" -> fbPagesIds))
                  db.command(Count(MongoDatabaseService.fbPageLikesCollection, Some(queryNotLiked))).onComplete {
                    case Success(count: Int) =>
                      finalizeStats(fbPosts, fbPages, count, userStats, itemsStats, itemsStatsCollection, userStatsCollection)
                    case Failure(e) =>
                      log.error(s"Could not reach database : $e")
                  }
                case Failure(e) =>
                  log.error(s"Could not reach database : $e")
              }
            case Failure(e) =>
              log.error(s"Could not reach database : $e")
          }
        case Failure(e) =>
          log.error(s"Could not reach database : $e")
      }
    } else {
      val selectOldItems = BSONDocument("userId" -> userId, "readForStats" -> false)
      val itemsStatsCursor = itemsStatsCollection.find(selectOldItems).cursor[ItemStats]
      itemsStatsCursor.collect[List](stopOnError = true).onComplete {
        case Success(itemsStats: List[ItemStats]) =>
          if (itemsStats.length > 0) {
            val postSelector = BSONDocument("userId" -> userId, "postId" -> BSONDocument("$in" -> itemsStats.map(is => is.itemId)))
            val postsCursor = postCollection.find(postSelector).cursor[FBPost]
            postsCursor.collect[List](itemsStats.length, stopOnError = true).onComplete {
              case Success(fbPosts: List[FBPost]) =>
                dealWithOldStats(fbPosts, itemsStats, userStats, itemsStatsCollection, userStatsCollection)
              case Failure(e) =>
                log.error(s"Could not reach database : $e")
            }
          }
        case Failure(e) =>
          log.error(s"Could not reach database : $e")
      }
    }
  }

  def finalizeStats(fbPosts: List[FBPost], fbPages: List[FBPage], unlikedPagesCount: Int, userStats: UserStats,
                    itemsStats: List[ItemStats], itemsStatsCollection: BSONCollection,
                    userStatsCollection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val newLikers = fbPosts.foldLeft(Set[FBLike]()) {
      (acc: Set[FBLike], post: FBPost) => {
        post.likes match {
          case Some(likes) => acc ++ likes.toSet
          case None => acc
        }
      }
    } ++ userStats.likers

    val updatedPosts: List[ItemStats] = fbPosts.flatMap {
      fbPost =>
        val likeNumber = fbPost.likesCount.getOrElse(0)
        if (newLikers.size - likeNumber >= 3 && likeNumber > 0) {
          val oldItemStat = getItemStats(userId, fbPost.postId, "Post", itemsStats)
          val newDataListing = oldItemStat.dataTypes.toSet + PostWhoLiked.name
          Some(ItemStats(None, userId, oldItemStat.itemId, "Post", newDataListing.toList, newDataListing.size))
        } else {
          None
        }
    }

    val updatedPages: List[ItemStats] = {
      val newDataListing =
        if (unlikedPagesCount >= 3) {
          List(Time.name, LikeNumber.name, PageWhichLiked.name)
        } else {
          List(Time.name, LikeNumber.name)
        }
      fbPages.map {
        fbPage =>
          ItemStats(None, userId, fbPage.pageId, "Page", newDataListing, newDataListing.size)
      }

    }

    val untouchedPosts = itemsStats.filterNot(
      is => updatedPosts.exists(ui => ui.userId == is.userId && ui.itemId == is.itemId)
    )

    val newItemsStats = (updatedPosts ++ untouchedPosts ++ updatedPages).map {
      is =>
        ItemStats(is.id, is.userId, is.itemId, is.itemType, is.dataTypes, is.dataCount, readForStats = true)
    }

    newItemsStats.foreach {
      itemStats =>
        val selector = BSONDocument("userId" -> userId, "itemId" -> itemStats.itemId)
        itemsStatsCollection.update(selector, itemStats, upsert = true)
    }

    val newUserStats = userStatsWithNewCounts(newLikers, newItemsStats, userStats)
    val selector = BSONDocument("userId" -> userId)
    userStatsCollection.update(selector, newUserStats, upsert = true)
  }

  // The stats in question can only be posts. Pages are only generated with the read flag to true as those items are
  // only generated during stats finalization.
  def dealWithOldStats(fbPosts: List[FBPost], itemsStats: List[ItemStats], userStats: UserStats,
                       itemsStatsCollection: BSONCollection, userStatsCollection: BSONCollection): Unit = {

    import scala.concurrent.ExecutionContext.Implicits.global
    val newLikers = fbPosts.foldLeft(Set[FBLike]()) {
      (acc: Set[FBLike], post: FBPost) => {
        post.likes match {
          case Some(likes) => acc ++ likes.toSet
          case None => acc
        }
      }
    } ++ userStats.likers

    val updatedPosts: List[ItemStats] = fbPosts.flatMap {
      fbPost =>
        val likeNumber = fbPost.likesCount.getOrElse(0)
        if (newLikers.size - likeNumber >= 3 && likeNumber > 0) {
          val oldItemStat = getItemStats(userId, fbPost.postId, "Post", itemsStats)
          val newDataListing = oldItemStat.dataTypes.toSet + PostWhoLiked.name
          Some(ItemStats(None, userId, oldItemStat.itemId, "Post", newDataListing.toList, newDataListing.size))
        } else {
          None
        }
    }


    val untouchedPosts = itemsStats.filterNot(
      is => updatedPosts.exists(ui => ui.userId == is.userId && ui.itemId == is.itemId)
    )

    val newItemsStats = (updatedPosts ++ untouchedPosts).map {
      is =>
        ItemStats(is.id, is.userId, is.itemId, is.itemType, is.dataTypes, is.dataCount, readForStats = true)
    }

    newItemsStats.foreach {
      itemStats =>
        val selector = BSONDocument("userId" -> userId, "itemId" -> itemStats.itemId)
        itemsStatsCollection.update(selector, itemStats, upsert = true)
    }

    val newUserStats = userStatsWithNewCounts(newLikers, newItemsStats, userStats)
    val selector = BSONDocument("userId" -> userId)
    userStatsCollection.update(selector, newUserStats, upsert = true)

  }
}
