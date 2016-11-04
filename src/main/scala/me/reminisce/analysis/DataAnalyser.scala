package me.reminisce.analysis

import akka.actor.{Actor, ActorLogging, Props}
import akka.event.{Logging, LoggingAdapter}
import me.reminisce.analysis.DataAnalyser._
import me.reminisce.analysis.DataTypes._
import me.reminisce.database.AnalysisEntities.{ItemSummary, UserSummary}
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities._
import me.reminisce.fetching.config.GraphResponses.Post
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind.Order
import me.reminisce.gameboard.questions.QuestionGenerationConfig
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/**
  * Factory for [[DataAnalyser]], case classes for message passing and useful methods
  */
object DataAnalyser {

  case class FinalAnalysis(fbPosts: Set[String], fbPages: Set[String])

  case class TransientPostsAnalysis(fbPosts: List[Post])

  /**
    * Creates a data analyser generator
    *
    * @param userId user for which the summary is generated
    * @param db     database holding the data
    * @return props for the created actor
    */
  def props(userId: String, db: DefaultDB): Props =
  Props(new DataAnalyser(userId, db))

  /**
    * Adds a new type (or some quantity of new items of a type already accounted for) to a map holding counts for each type
    *
    * @param typeAndCount new type
    * @param oldMap       map containing counts
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
    *
    * @param typesAndCounts new type
    * @param oldMap         map containing counts
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
    * stored posts (this is partial analysis).
    *
    * @param post post to handle
    * @return a list of data types
    */
  def availableDataTypes(post: Post): List[DataType] = {
    List(hasTimeData(post), hasGeolocationData(post), hasWhoCommentedData(post),
      hasCommentNumber(post), hasLikeNumber(post)).flatten
  }

  /**
    * Checks if post has time data
    *
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
    *
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
    *
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
    *
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
    *
    * @param post post to handle
    * @return a data type option
    */
  private def hasLikeNumber(post: Post): Option[DataType] = {
    val likeCount = post.reactions.flatMap(root => root.data).getOrElse(List()).size
    if (likeCount > 0) {
      Some(LikeNumber)
    } else {
      None
    }
  }

  /**
    * Get item summary for user id and item id in a list of item summaries. Returns a default value if nothing is found
    *
    * @param userId         user to look for
    * @param itemId         item to look for
    * @param itemType       type of the item
    * @param itemsSummaries list of items summaries
    * @return found item summary
    */
  def getItemSummary(userId: String, itemId: String, itemType: String, itemsSummaries: List[ItemSummary]): ItemSummary = {
    itemsSummaries.filter(is => is.userId == userId && is.itemId == itemId) match {
      case Nil =>
        ItemSummary(None, userId, itemId, itemType, List(), 0)
      case head :: tail =>
        //Normally only one match is possible
        head
    }
  }

  /**
    * Aggregates new post likers and new items summaries and the old counts in the old user summary to create new user summary
    *
    * @param newLikers         new post likers
    * @param newItemsSummaries new items summaries
    * @param userSummary       old user summary
    * @return new user summary
    */
  def userSummaryWithNewCounts(newLikers: Set[FBReaction], newItemsSummaries: List[ItemSummary], userSummary: UserSummary): UserSummary = {
    val newDataTypes = newItemsSummaries.foldLeft(userSummary.dataTypeCounts) {
      case (acc, itemSummary) => addTypesToMap(itemSummary.dataTypes.map(dType => (dType, 1)), acc)
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

    UserSummary(userSummary.id, userSummary.userId, newDataTypes, newQuestionCounts, newLikers)
  }

}

/**
  * Data Analyser
  *
  * @param userId user for which the data summary is computed
  * @param db     database to store the summaries in
  */
class DataAnalyser(userId: String, db: DefaultDB) extends Actor with ActorLogging {

  override val log: LoggingAdapter = Logging(context.system, this)

  /**
    * Entry point of this actor. Handles the following messages:
    * - FinalAnalysis(fbPosts, fbPages): produces a user summary using old user summary and the not read items summaries
    * in the database
    * - TransientPostsAnalysis(fbPosts): save partial summary for posts
    *
    * @return Nothing
    */
  def receive = {
    case FinalAnalysis(fbPosts, fbPages) =>
      val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)
      val itemsSummariesCollection = db[BSONCollection](MongoCollections.itemsSummaries)
      val postCollection = db[BSONCollection](MongoCollections.fbPosts)
      val pagesCollection = db[BSONCollection](MongoCollections.fbPages)
      val selector = BSONDocument("userId" -> userId)
      userSummariesCollection.find(selector).one[UserSummary].onComplete {
        case Success(maybeUserSummary) =>
          lazy val emptyUserSummary = UserSummary(None, userId, Map(), Map(), Set())
          finalizeSummary(fbPosts.toList, fbPages.toList, maybeUserSummary.getOrElse(emptyUserSummary), postCollection,
            pagesCollection, userSummariesCollection, itemsSummariesCollection)
        case Failure(e) =>
          log.error(s"Database could not be reached : $e.")
        case any =>
          log.error(s"Unknown database error: $any.")
      }
    case TransientPostsAnalysis(fbPosts) =>
      saveTransientPostsSummary(fbPosts, db[BSONCollection](MongoCollections.itemsSummaries))
    case any =>
      log.error("DataAnalyser received unhandled message : " + any)
  }


  /**
    * Stores the available data types (partial) for posts into the database
    *
    * @param fbPosts                  posts to analyse
    * @param itemsSummariesCollection collection in which to store the summaries
    */
  private def saveTransientPostsSummary(fbPosts: List[Post], itemsSummariesCollection: BSONCollection): Unit = {
    fbPosts.foreach {
      post =>
        val selector = BSONDocument("userId" -> userId, "itemId" -> post.id)
        val availableData = availableDataTypes(post).map(dType => dType.name)
        val itemSummary = ItemSummary(None, userId, post.id, "Post", availableData, availableData.length)
        itemsSummariesCollection.update(selector, itemSummary, upsert = true)
    }

  }

  /**
    * If there are posts or pages in fbPostsIds and fbPagesIds, use those items to finalize the summary (this time taking
    * into account everything such as post likers, number of similar data types to compute the number of order questions
    * etc...) otherwise only aggregates the not read ones in the database (initially the items summaries are stored as not
    * read because they were not aggregated completely with old user summary).
    *
    * @param fbPostsIds               ids of posts to handle
    * @param fbPagesIds               ids of pages to handle
    * @param userSummary              old user summary
    * @param postCollection           collection containing posts
    * @param pagesCollection          collection containing pages
    * @param userSummaryCollection    collection containing user summaries
    * @param itemsSummariesCollection collection containing items summaries
    */
  private def finalizeSummary(fbPostsIds: List[String], fbPagesIds: List[String], userSummary: UserSummary,
                              postCollection: BSONCollection, pagesCollection: BSONCollection, userSummaryCollection: BSONCollection,
                              itemsSummariesCollection: BSONCollection): Unit = {
    if ((fbPagesIds ++ fbPostsIds).nonEmpty) {
      val postSelector = BSONDocument("userId" -> userId, "postId" -> BSONDocument("$in" -> fbPostsIds))
      val postsCursor = postCollection.find(postSelector).cursor[FBPost]()
      (for {
        fbPosts <- postsCursor.collect[List](fbPostsIds.length, stopOnError = true)

        pageSelector = BSONDocument("pageId" -> BSONDocument("$in" -> fbPagesIds))
        pagesCursor = pagesCollection.find(pageSelector).cursor[FBPage]()
        fbPages <- pagesCursor.collect[List](fbPagesIds.length, stopOnError = true)

        itemSummarySelector = BSONDocument("userId" -> userId, "itemId" -> BSONDocument("$in" -> fbPostsIds))
        itemsSummariesCursor = itemsSummariesCollection.find(itemSummarySelector).cursor[ItemSummary]()
        itemSummaries <- itemsSummariesCursor.collect[List](fbPostsIds.length, stopOnError = true)

        queryNotLiked = BSONDocument("userId" -> userId, "pageId" -> BSONDocument("$nin" -> fbPagesIds))
        collection = db[BSONCollection](MongoCollections.fbPageLikes)
        notLikedPagesCount <- collection.count(Some(queryNotLiked))
      } yield finalizeSummary(fbPosts, fbPages, notLikedPagesCount, userSummary, itemSummaries, itemsSummariesCollection, userSummaryCollection)
        ) onFailure {
        case e =>
          log.error(s"Could not reach database : $e")
      }
    } else {
      val selectOldItems = BSONDocument("userId" -> userId, "readForSummary" -> false)
      val itemsSummariesCursor = itemsSummariesCollection.find(selectOldItems).cursor[ItemSummary]()
      (for {
        itemSummaries <- itemsSummariesCursor.collect[List](stopOnError = true)

        postSelector = BSONDocument("userId" -> userId, "postId" -> BSONDocument("$in" -> itemSummaries.map(is => is.itemId)))
        postsCursor = postCollection.find(postSelector).cursor[FBPost]()
        fbPosts <- postsCursor.collect[List](itemSummaries.length, stopOnError = true) if itemSummaries.nonEmpty
      } yield dealWithOldSummaries(fbPosts, itemSummaries, userSummary, itemsSummariesCollection, userSummaryCollection)
        ) onFailure {
        case e =>
          e.getMessage match {
            case "Future.filter predicate is not satisfied" =>
              log.info("There was no element in old summaries.")
            case any =>
              log.error(s"Could not reach database : $e")
          }
      }
    }
  }

  /**
    * Concretely performs the aggregation
    *
    * @param fbPosts                  posts to handle
    * @param fbPages                  pages to handle
    * @param notLikedPagesCount       number of pages not liked
    * @param userSummary              old user summary
    * @param itemSummaries            old items summaries
    * @param itemsSummariesCollection collection for items summaries
    * @param userSummariesCollection  collection for user summaries
    */
  private def finalizeSummary(fbPosts: List[FBPost], fbPages: List[FBPage], notLikedPagesCount: Int, userSummary: UserSummary,
                              itemSummaries: List[ItemSummary], itemsSummariesCollection: BSONCollection,
                              userSummariesCollection: BSONCollection): Unit = {
    val newLikers = accumulateLikes(fbPosts) ++ userSummary.likers

    val updatedPosts: List[ItemSummary] = updatePostsSummaries(fbPosts, newLikers, itemSummaries)

    val updatedPages: List[ItemSummary] = updatePagesSummaries(fbPages, notLikedPagesCount)

    val untouchedPosts = itemSummaries.filterNot(
      is => updatedPosts.exists(ui => ui.userId == is.userId && ui.itemId == is.itemId)
    )

    val newItemsSummaries = (updatedPosts ++ untouchedPosts ++ updatedPages).map {
      is =>
        ItemSummary(is.id, is.userId, is.itemId, is.itemType, is.dataTypes, is.dataCount, readForSummary = true)
    }

    newItemsSummaries.foreach {
      itemSummary =>
        val selector = BSONDocument("userId" -> userId, "itemId" -> itemSummary.itemId)
        itemsSummariesCollection.update(selector, itemSummary, upsert = true)
    }

    val newUserSummaries = userSummaryWithNewCounts(newLikers, newItemsSummaries, userSummary)
    val selector = BSONDocument("userId" -> userId)
    userSummariesCollection.update(selector, newUserSummaries, upsert = true)
  }

  /**
    * Aggregates not read items summaries with the user summary.
    *
    * @param fbPosts                  handled posts
    * @param itemSummaries            not read items summaries
    * @param userSummary              old user summary
    * @param itemsSummariesCollection collection containing items summaries
    * @param userSummariesCollection  collection containing user summaries
    */
  private def dealWithOldSummaries(fbPosts: List[FBPost], itemSummaries: List[ItemSummary], userSummary: UserSummary,
                                   itemsSummariesCollection: BSONCollection, userSummariesCollection: BSONCollection): Unit = {

    val newLikers = accumulateLikes(fbPosts) ++ userSummary.likers

    val updatedPosts: List[ItemSummary] = updatePostsSummaries(fbPosts, newLikers, itemSummaries)


    val untouchedPosts = itemSummaries.filterNot(
      is => updatedPosts.exists(ui => ui.userId == is.userId && ui.itemId == is.itemId)
    )

    val newItemsSummaries = (updatedPosts ++ untouchedPosts).map {
      is =>
        ItemSummary(is.id, is.userId, is.itemId, is.itemType, is.dataTypes, is.dataCount, readForSummary = true)
    }

    newItemsSummaries.foreach {
      itemSummary =>
        val selector = BSONDocument("userId" -> userId, "itemId" -> itemSummary.itemId)
        itemsSummariesCollection.update(selector, itemSummary, upsert = true)
    }

    val newUserSummary = userSummaryWithNewCounts(newLikers, newItemsSummaries, userSummary)
    val selector = BSONDocument("userId" -> userId)
    userSummariesCollection.update(selector, newUserSummary, upsert = true)

  }

  /**
    * Gets likes from a list of posts
    *
    * @param fbPosts posts to handle
    * @return list of likes
    */
  private def accumulateLikes(fbPosts: List[FBPost]): Set[FBReaction] = {
    fbPosts.foldLeft(Set[FBReaction]()) {
      (acc: Set[FBReaction], post: FBPost) => {
        post.reactions match {
          case Some(reactions) => acc ++ reactions.toSet
          case None => acc
        }
      }
    }
  }

  /**
    * Update post items summaries based on the number of likers
    *
    * @param fbPosts       posts to handle
    * @param likers        likers
    * @param itemSummaries items summaries to update
    * @return list of updated items summaries
    */
  private def updatePostsSummaries(fbPosts: List[FBPost], likers: Set[FBReaction],
                                   itemSummaries: List[ItemSummary]): List[ItemSummary] = {
    fbPosts.flatMap {
      fbPost =>
        val likeNumber = fbPost.reactionCount.getOrElse(0)
        if (likers.size - likeNumber >= 3 && likeNumber > 0) {
          val oldItemStat = getItemSummary(userId, fbPost.postId, "Post", itemSummaries)
          val newDataListing = oldItemStat.dataTypes.toSet + PostWhoReacted.name
          Some(ItemSummary(None, userId, oldItemStat.itemId, "Post", newDataListing.toList, newDataListing.size))
        } else {
          None
        }
    }
  }

  /**
    * Updates pages summaries based on the number of not liked pages
    *
    * @param fbPages            pages to handle
    * @param notLikedPagesCount number of not liked pages
    * @return list of items summaries
    */
  private def updatePagesSummaries(fbPages: List[FBPage], notLikedPagesCount: Int): List[ItemSummary] = {
    val newDataListing =
      if (notLikedPagesCount >= 3) {
        List(Time.name, LikeNumber.name, PageWhichLiked.name)
      } else {
        List(Time.name, LikeNumber.name)
      }
    fbPages.map {
      fbPage =>
        ItemSummary(None, userId, fbPage.pageId, "Page", newDataListing, newDataListing.size)
    }
  }
}
