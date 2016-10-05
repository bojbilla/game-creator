package me.reminisce.stats

import akka.actor.{Actor, ActorLogging, Props}
import akka.event.{Logging, LoggingAdapter}
import me.reminisce.database.MongoDBEntities._
import me.reminisce.database.StatsEntities.{ItemStats, UserStats}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.fetching.config.GraphResponses.Post
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind.Order
import me.reminisce.gameboard.questions.QuestionGenerationConfig
import me.reminisce.stats.StatsDataTypes._
import me.reminisce.stats.StatsGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.Count

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/**
  * Factory for [[me.reminisce.stats.StatsGenerator]], case classes for message passing and useful methods
  */
object StatsGenerator {

  case class FinalStats(fbPosts: Set[String], fbPages: Set[String])

  case class TransientPostsStats(fbPosts: List[Post])

  /**
    * Create a stats generator
    * @param userId user for which the stats are generated
    * @param db database holding the data
    * @return props for the created actor
    */
  def props(userId: String, db: DefaultDB): Props =
    Props(new StatsGenerator(userId, db))

  /**
    * Adds a new type (or some quantity of new items of a type already accounted for) to a map holding counts for each type
    * @param typeAndCount new type
    * @param oldMap map containing counts
    * @return new map
    */
  private def addTypeToMap(typeAndCount: (String, Int), oldMap: Map[String, Int]): Map[String, Int] = typeAndCount match {
    case (tpe, count) =>
      if (oldMap.contains(tpe)) {
        oldMap.updated(tpe, oldMap(tpe) + count)
      } else {
        oldMap.updated(tpe, count)
      }
  }

  /**
    * Adds new types  (or some quantity of new items of types already accounted for) to a map holding counts for each type
    * @param typesAndCounts new type
    * @param oldMap map containing counts
    * @return new map
    */
  @tailrec
  def addTypesToMap(typesAndCounts: List[(String, Int)], oldMap: Map[String, Int]): Map[String, Int] = {
    typesAndCounts match {
      case Nil => oldMap
      case x :: xs => addTypesToMap(xs, addTypeToMap(x, oldMap))
    }
  }

  /**
    * gets the available data types on a given post. Only uses the post itself and odes NOT take into account already
    * stored posts (this is partial stats).
    * @param post post to handle
    * @return a list of data types
    */
  def availableDataTypes(post: Post): List[DataType] = {
    List(hasTimeData(post), hasGeolocationData(post), hasWhoCommentedData(post),
      hasCommentNumber(post), hasLikeNumber(post)).flatten
  }

  /**
    * Checks if post has time data
    * @param post post to handle
    * @return a data type option
    */
  private def hasTimeData(post: Post): Option[DataType] = {
    if ((post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty)) && post.created_time.nonEmpty)
      Some(Time)
    else
      None
  }

  /**
    * Checks if post has geolocation data
    * @param post post to handle
    * @return a data type option
    */
  private def hasGeolocationData(post: Post): Option[DataType] = {
    post.place.flatMap(place => place.location.flatMap(
      location => location.latitude.flatMap(lat => location.longitude.map(
        long => PostGeolocation
      ))
    ))
  }

  /**
    * Checks if post has enough comments for a "who commented" question
    * @param post post to handle
    * @return a data type option
    */
  private def hasWhoCommentedData(post: Post): Option[DataType] = {
    val fbComments = post.comments.flatMap(root => root.data.map(comments => comments.map { c =>
      FBComment(c.id, FBFrom(c.from.id, c.from.name), c.like_count, c.message)
    }))
    if (post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty)) {
      for {
        comments <- fbComments
        fromSet = comments.map(comm => comm.from).toSet
        if fromSet.size > 3
      } yield PostWhoCommented
    } else {
      None
    }
  }

  /**
    * Checks if post has comments
    * @param post post to handle
    * @return a data type option
    */
  private def hasCommentNumber(post: Post): Option[DataType] = {
    val fbComments = post.comments.flatMap(root => root.data.map(comments => comments.map { c =>
      FBComment(c.id, FBFrom(c.from.id, c.from.name), c.like_count, c.message)
    }))
    if ((post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty)) && fbComments.nonEmpty) {
      fbComments.withFilter(comments => comments.size > 3).map(comments => PostCommentsNumber)
    } else {
      None
    }
  }

  /**
    * Checks if post has likes
    * @param post post to handle
    * @return a data type option
    */
  private def hasLikeNumber(post: Post): Option[DataType] = {
    val likeCount = post.likes.flatMap(root => root.data).getOrElse(List()).size
    if (likeCount > 0) {
      Some(LikeNumber)
    } else {
      None
    }
  }

  /**
    * Get item stats for user id and item id in a list of item stats. Returns a default value if nothing is found
    * @param userId user to look for
    * @param itemId item to look for
    * @param itemType type of the item
    * @param itemsStats list of items stats
    * @return found item stats
    */
  def getItemStats(userId: String, itemId: String, itemType: String, itemsStats: List[ItemStats]): ItemStats = {
    itemsStats.filter(is => is.userId == userId && is.itemId == itemId) match {
      case Nil =>
        ItemStats(None, userId, itemId, itemType, List(), 0)
      case head :: tail =>
        //Normally only one match is possible
        head
    }
  }

  /**
    * Aggregates new post likers and new items stats and the old counts in the old user stats to create new user stats
    * @param newLikers new post likers
    * @param newItemsStats new items stats
    * @param userStats old user stats
    * @return new user stats
    */
  def userStatsWithNewCounts(newLikers: Set[FBLike], newItemsStats: List[ItemStats], userStats: UserStats): UserStats = {
    val newDataTypes = newItemsStats.foldLeft(userStats.dataTypeCounts) {
      case (acc, itemStats) => addTypesToMap(itemStats.dataTypes.map(dType => (dType, 1)), acc)
    }

    // One has to be careful as the count for order is just the count of items that have a data type suited for ordering
    // Ordering have to be a multiple of the number of items to order
    val newQuestionCounts: Map[String, Int] = newDataTypes.foldLeft(Map[String, Int]()) {
      case (acc, (tpe, cnt)) =>
        val kinds = possibleKind(stringToType(tpe))
        val newCounts = kinds.map {
          kind =>
            val count = kind match {
              case Order =>
                val excess = cnt % QuestionGenerationConfig.orderingItemsNumber
                cnt - excess
              case _ =>
                cnt
            }
            (kind.toString, count)
        }
        addTypesToMap(newCounts, acc)
    }

    UserStats(userStats.id, userStats.userId, newDataTypes, newQuestionCounts, newLikers)
  }

}

/**
  * Statistics generator
  * @param userId user for which the statistics are computed
  * @param db database to store the stats in
  */
class StatsGenerator(userId: String, db: DefaultDB) extends Actor with ActorLogging {

  override val log: LoggingAdapter = Logging(context.system, this)

  /**
    * Entry point of this actor. Handles the following messages:
    * - FinalStats(fbPosts, fbPages): aggregates statistics using old user stats and the not read items stats in the
    * database
    * - TransientPostsStats(fbPosts): save partial stats for posts
    * @return Nothing
    */
  def receive = {
    case FinalStats(fbPosts, fbPages) =>
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
        case any =>
          log.error(s"Unknown database error: $any.")
      }
    case TransientPostsStats(fbPosts) =>
      saveTransientPostsStats(fbPosts, db[BSONCollection](MongoDatabaseService.itemsStatsCollection))
    case any =>
      log.error("StatsHandler received unhandled message : " + any)
  }


  /**
    * Stores the available data types (partial) for posts into the database
    * @param fbPosts posts to analyse
    * @param itemsStatsCollection collection in which to store the stats
    */
  private def saveTransientPostsStats(fbPosts: List[Post], itemsStatsCollection: BSONCollection): Unit = {
    fbPosts.foreach {
      post =>
        val selector = BSONDocument("userId" -> userId, "itemId" -> post.id)
        val availableData = availableDataTypes(post).map(dType => dType.name)
        val itemStats = ItemStats(None, userId, post.id, "Post", availableData, availableData.length)
        itemsStatsCollection.update(selector, itemStats, upsert = true)
    }

  }

  /**
    * If there are posts or pages in fbPostsIds and fbPagesIds, use those items to finalize the stats (this time taking
    * into account everything such as post likers, number of similar data types to compute the number of order questions
    * etc...) otherwise only aggregates the not read ones in the database (initially the items stats are stored as not
    * read because they were not aggregated completely with old user stats).
    * @param fbPostsIds ids of posts to handle
    * @param fbPagesIds ids of pages to handle
    * @param userStats old user stats
    * @param postCollection collection containing posts
    * @param pagesCollection collection containing pages
    * @param userStatsCollection collection containing user stats
    * @param itemsStatsCollection collection containing items stats
    */
  private def finalizeStats(fbPostsIds: List[String], fbPagesIds: List[String], userStats: UserStats,
                            postCollection: BSONCollection, pagesCollection: BSONCollection, userStatsCollection: BSONCollection,
                            itemsStatsCollection: BSONCollection): Unit = {
    if ((fbPagesIds ++ fbPostsIds).nonEmpty) {
      val postSelector = BSONDocument("userId" -> userId, "postId" -> BSONDocument("$in" -> fbPostsIds))
      val postsCursor = postCollection.find(postSelector).cursor[FBPost]()
      (for {
        fbPosts <- postsCursor.collect[List](fbPostsIds.length, stopOnError = true)

        pageSelector = BSONDocument("pageId" -> BSONDocument("$in" -> fbPagesIds))
        pagesCursor = pagesCollection.find(pageSelector).cursor[FBPage]()
        fbPages <- pagesCursor.collect[List](fbPagesIds.length, stopOnError = true)

        itemStatsSelector = BSONDocument("userId" -> userId, "itemId" -> BSONDocument("$in" -> fbPostsIds))
        itemsStatsCursor = itemsStatsCollection.find(itemStatsSelector).cursor[ItemStats]()
        itemsStats <- itemsStatsCursor.collect[List](fbPostsIds.length, stopOnError = true)

        queryNotLiked = BSONDocument("userId" -> userId, "pageId" -> BSONDocument("$nin" -> fbPagesIds))
        collection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
        notLikedPagesCount <- collection.count(Some(queryNotLiked))
      } yield finalizeStats(fbPosts, fbPages, notLikedPagesCount, userStats, itemsStats, itemsStatsCollection, userStatsCollection)
        ) onFailure {
        case e =>
          log.error(s"Could not reach database : $e")
      }
    } else {
      val selectOldItems = BSONDocument("userId" -> userId, "readForStats" -> false)
      val itemsStatsCursor = itemsStatsCollection.find(selectOldItems).cursor[ItemStats]()
      (for {
        itemsStats <- itemsStatsCursor.collect[List](stopOnError = true)

        postSelector = BSONDocument("userId" -> userId, "postId" -> BSONDocument("$in" -> itemsStats.map(is => is.itemId)))
        postsCursor = postCollection.find(postSelector).cursor[FBPost]()
        fbPosts <- postsCursor.collect[List](itemsStats.length, stopOnError = true) if itemsStats.nonEmpty
      } yield dealWithOldStats(fbPosts, itemsStats, userStats, itemsStatsCollection, userStatsCollection)
        ) onFailure {
        case e =>
          e.getMessage match {
            case "Future.filter predicate is not satisfied" =>
              log.info("There was no element in old stats.")
            case any =>
              log.error(s"Could not reach database : $e")
          }
      }
    }
  }

  /**
    * Concretely performs the aggregation
    * @param fbPosts posts to handle
    * @param fbPages pages to handle
    * @param notLikedPagesCount number of pages not liked
    * @param userStats old user stats
    * @param itemsStats old items stats
    * @param itemsStatsCollection collection for items stats
    * @param userStatsCollection collection for user stats
    */
  private def finalizeStats(fbPosts: List[FBPost], fbPages: List[FBPage], notLikedPagesCount: Int, userStats: UserStats,
                            itemsStats: List[ItemStats], itemsStatsCollection: BSONCollection,
                            userStatsCollection: BSONCollection): Unit = {
    val newLikers = accumulateLikes(fbPosts) ++ userStats.likers

    val updatedPosts: List[ItemStats] = updatePostsStats(fbPosts, newLikers, itemsStats)

    val updatedPages: List[ItemStats] = updatePagesStats(fbPages, notLikedPagesCount)

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

  /**
    * Aggregates not read items stats with the user stats.
    * @param fbPosts handled posts
    * @param itemsStats not read items stats
    * @param userStats old user stats
    * @param itemsStatsCollection collection containing items stats
    * @param userStatsCollection collection containing user stats
    */
  private def dealWithOldStats(fbPosts: List[FBPost], itemsStats: List[ItemStats], userStats: UserStats,
                               itemsStatsCollection: BSONCollection, userStatsCollection: BSONCollection): Unit = {

    val newLikers = accumulateLikes(fbPosts) ++ userStats.likers

    val updatedPosts: List[ItemStats] = updatePostsStats(fbPosts, newLikers, itemsStats)


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

  /**
    * Gets likes from a list of posts
    * @param fbPosts posts to handle
    * @return list of likes
    */
  private def accumulateLikes(fbPosts: List[FBPost]): Set[FBLike] = {
    fbPosts.foldLeft(Set[FBLike]()) {
      (acc: Set[FBLike], post: FBPost) => {
        post.likes match {
          case Some(likes) => acc ++ likes.toSet
          case None => acc
        }
      }
    }
  }

  /**
    * Update post items stats based on the number of likers
    * @param fbPosts posts to handle
    * @param likers likers
    * @param itemsStats items stats to update
    * @return list of updated items stats
    */
  private def updatePostsStats(fbPosts: List[FBPost], likers: Set[FBLike],
                               itemsStats: List[ItemStats]): List[ItemStats] = {
    fbPosts.flatMap {
      fbPost =>
        val likeNumber = fbPost.likesCount.getOrElse(0)
        if (likers.size - likeNumber >= 3 && likeNumber > 0) {
          val oldItemStat = getItemStats(userId, fbPost.postId, "Post", itemsStats)
          val newDataListing = oldItemStat.dataTypes.toSet + PostWhoLiked.name
          Some(ItemStats(None, userId, oldItemStat.itemId, "Post", newDataListing.toList, newDataListing.size))
        } else {
          None
        }
    }
  }

  /**
    * Updates pages stats based on the number of not liked pages
    * @param fbPages pages to handle
    * @param notLikedPagesCount number of not liked pages
    * @return list of items stats
    */
  private def updatePagesStats(fbPages: List[FBPage], notLikedPagesCount: Int): List[ItemStats] = {
    val newDataListing =
      if (notLikedPagesCount >= 3) {
        List(Time.name, LikeNumber.name, PageWhichLiked.name)
      } else {
        List(Time.name, LikeNumber.name)
      }
    fbPages.map {
      fbPage =>
        ItemStats(None, userId, fbPage.pageId, "Page", newDataListing, newDataListing.size)
    }
  }
}
