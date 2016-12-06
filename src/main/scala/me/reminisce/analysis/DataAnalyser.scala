package me.reminisce.analysis

import java.util.concurrent.ConcurrentHashMap

import akka.actor.{Actor, ActorLogging, PoisonPill, Props}
import akka.event.{Logging, LoggingAdapter}
import me.reminisce.analysis.DataAnalyser._
import me.reminisce.analysis.DataTypes._
import me.reminisce.database.AnalysisEntities.{ItemSummary, UserSummary}
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities._
import me.reminisce.fetching.config.GraphResponses.{Friend, Post}
import me.reminisce.gameboard.board.GameboardEntities.{Order, QuestionKind}
import me.reminisce.gameboard.questions.QuestionGenerationConfig
import me.reminisce.server.domain.Domain.{AckBlackList, FailedBlacklist}
import me.reminisce.server.domain.RestMessage
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Factory for [[DataAnalyser]], case classes for message passing and useful methods
  */
object DataAnalyser {

  case class FinalAnalysis(fbPosts: Set[String], fbPages: Set[String], fbFriends: Set[Friend], retries: Int = 0)

  case class TransientPostsAnalysis(fbPosts: List[Post])

  case class NewBlackList(blacklist: Set[FBFrom], retries: Int = 0) extends RestMessage

  /**
    * Creates a data analyser generator
    *
    * @param userId user for which the summary is generated
    * @param db     database holding the data
    * @return props for the created actor
    */
  def props(userId: String, db: DefaultDB): Props =
  Props(new DataAnalyser(userId, db))

  private val transientDataTypes = Set[DataType](Time, PostGeolocation, PostCommentsNumber, PostReactionNumber)

  private val analysingFor = new ConcurrentHashMap[String, Boolean]()

  /**
    * Add a new count to a map of items => count
    *
    * @param typeAndCount new count
    * @param oldMap       current counts
    * @tparam T the type of item being counted
    * @return the new map
    */
  private def addTypeToMap[T](typeAndCount: (T, Int), oldMap: Map[T, Int]): Map[T, Int] = typeAndCount match {
    case (tpe, count) =>
      if (oldMap.contains(tpe)) {
        oldMap.updated(tpe, oldMap(tpe) + count)
      } else {
        oldMap.updated(tpe, count)
      }
  }

  /**
    * Adds new counts to a map of item => count
    *
    * @param typesAndCounts new counts
    * @param oldMap         current counts
    * @tparam T type of item counted
    * @return the new map
    */
  @tailrec
  def addTypesToMap[T](typesAndCounts: Iterable[(T, Int)], oldMap: Map[T, Int]): Map[T, Int] = {
    if (typesAndCounts.isEmpty) {
      oldMap
    } else {
      addTypesToMap(typesAndCounts.tail, addTypeToMap(typesAndCounts.head, oldMap))
    }
  }

  /**
    * gets the available data types on a given post. Only uses the post itself and odes NOT take into account already
    * stored posts (this is partial analysis).
    *
    * @param post post to handle
    * @return a list of data types
    */
  def availableDataTypes(post: Post): Set[DataType] = {
    Set(hasTimeData(post), hasGeolocationData(post), hasCommentNumber(post), hasReactionsNumber(post)).flatten
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
    * Checks if post has reactions
    *
    * @param post post to handle
    * @return a data type option
    */
  private def hasReactionsNumber(post: Post): Option[DataType] = {
    val reactionsCount = post.reactions.flatMap(root => root.data).getOrElse(List()).size
    if (reactionsCount > 0) {
      Some(PostReactionNumber)
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
  def getItemSummary(userId: String, itemId: String, itemType: ItemType, itemsSummaries: List[ItemSummary]): ItemSummary = {
    itemsSummaries.filter(is => is.userId == userId && is.itemId == itemId) match {
      case Nil =>
        ItemSummary(None, userId, itemId, itemType, Set(), 0)
      case head :: tail =>
        //Normally only one match is possible
        head
    }
  }

  /**
    * Aggregates new post reactioners and new items summaries and the old counts in the old user summary to create new user summary
    *
    * @param newReactioners    new post reactioners
    * @param newItemsSummaries new items summaries
    * @param userSummary       old user summary
    * @param friends           found friends
    * @return new user summary
    */
  def userSummaryWithNewCounts(newReactioners: Set[AbstractReaction], newItemsSummaries: List[ItemSummary], friends: Set[Friend],
                               userSummary: UserSummary): UserSummary = {
    val newDataTypes = newItemsSummaries.foldLeft(userSummary.dataTypeCounts) {
      case (acc, itemSummary) => addTypesToMap[DataType](itemSummary.dataTypes.map(dType => (dType, 1)), acc)
    }

    // One has to be careful as the count for order is just the count of items that have a data type suited for ordering
    // Ordering have to be a multiple of the number of items to order
    val newQuestionCounts: Map[QuestionKind, Int] = newDataTypes.foldLeft(Map[QuestionKind, Int]()) {
      case (acc, (tpe, cnt)) =>
        val kinds = typeToKinds.getOrElse(tpe, List())
        val newCounts = kinds.map {
          kind =>
            val count = kind match {
              case Order =>
                val excess = cnt % QuestionGenerationConfig.orderingItemsNumber
                cnt - excess
              case _ =>
                cnt
            }
            (kind, count)
        }
        addTypesToMap[QuestionKind](newCounts, acc)
    }

    val newFbFriends = friends.map(FBFriend(_))

    userSummary.copy(dataTypeCounts = newDataTypes, questionCounts = newQuestionCounts, reactioners = newReactioners,
      friends = newFbFriends)
  }

  def applyBlacklist[React <: AbstractReaction](reactions: Set[React], blacklist: Set[FBFrom]): Set[React] = {
    reactions.filterNot(react => blacklist.contains(react.from))
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
    case FinalAnalysis(fbPosts, fbPages, fbFriends, retries) =>
      if (!analysingFor.putIfAbsent(userId, true)) {
        handleWithUserSummary {
          userSummary =>
            finalizeSummary(fbPosts.toList, fbPages.toList, fbFriends, userSummary)
        }
      } else {
        if (retries < 100) {
          context.system.scheduler.scheduleOnce(200.milliseconds, self, FinalAnalysis(fbPosts, fbPages, fbFriends, retries + 1))
        } else {
          log.error(s"Could not perform final analysis for $userId: too many retries ($retries).")
        }
      }
    case TransientPostsAnalysis(fbPosts) =>
      saveTransientPostsSummary(fbPosts, db[BSONCollection](MongoCollections.itemsSummaries))
    case NewBlackList(blacklist, retries) =>
      val client = sender()
      if (!analysingFor.putIfAbsent(userId, true)) {
        client ! AckBlackList(s"Updating blacklist for user $userId.")
        handleWithUserSummary {
          userSummary =>
            val newSummary = userSummary.copy(dataTypeCounts = Map(), questionCounts = Map(), blacklist = Some(blacklist))
            updateBlackList(newSummary)
        }
      } else {
        if (retries < 100) {
          context.system.scheduler.scheduleOnce(200.milliseconds, self, NewBlackList(blacklist, retries + 1))
        } else {
          client ! FailedBlacklist(s"Updating blacklist for user $userId.")
          log.error(s"Could not update blacklist for $userId: too many retries ($retries).")
        }
      }
    case any =>
      log.error("DataAnalyser received unhandled message : " + any)
  }

  private def handleWithUserSummary(handle: UserSummary => Unit): Unit = {
    val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)
    val selector = BSONDocument("userId" -> userId)
    userSummariesCollection.find(selector).one[UserSummary].onComplete {
      case Success(maybeUserSummary) =>
        lazy val emptyUserSummary = UserSummary(None, userId, Map(), Map(), Set(), Set(), None)
        handle(maybeUserSummary.getOrElse(emptyUserSummary))
      case Failure(e) =>
        log.error(s"Database could not be reached : $e.")
        analysingFor.remove(userId)
      case any =>
        log.error(s"Unknown database error: $any.")
        analysingFor.remove(userId)
    }
  }


  private def updateBlackList(userSummary: UserSummary): Unit = {
    val postCollection = db[BSONCollection](MongoCollections.fbPosts)
    val pagesCollection = db[BSONCollection](MongoCollections.fbPages)
    val itemsSummariesCollection = db[BSONCollection](MongoCollections.itemsSummaries)
    val pageLikesCollection = db[BSONCollection](MongoCollections.fbPageLikes)
    val selector = BSONDocument("userId" -> userId)
    val postsCursor = postCollection.find(selector).cursor[FBPost]()
    (for {
      fbPosts <- postsCursor.collect[List](stopOnError = true)

      likedPagesCursor = pageLikesCollection.find(selector).cursor[FBPageLike]()
      likedPages <- likedPagesCursor.collect[List](stopOnError = true)
      likedPagesIds = likedPages.map(pageLike => pageLike.pageId)

      pageSelector = BSONDocument("pageId" -> BSONDocument("$in" -> likedPagesIds))
      pagesCursor = pagesCollection.find(pageSelector).cursor[FBPage]()
      fbPages <- pagesCursor.collect[List](likedPagesIds.length, stopOnError = true)

      itemSummarySelector = BSONDocument("userId" -> userId, "itemType" -> "Post")
      itemsSummariesCursor = itemsSummariesCollection.find(itemSummarySelector).cursor[ItemSummary]()
      itemSummaries <- itemsSummariesCursor.collect[List](stopOnError = true)

      queryNotLiked = BSONDocument("userId" -> userId, "pageId" -> BSONDocument("$nin" -> likedPages))
      notLikedPagesCount <- pageLikesCollection.count(Some(queryNotLiked))
    } yield {
      val friends = userSummary.friends.map(friend => Friend(friend.name, friend.name))
      val cleanedSummaries = itemSummaries.map {
        summary =>
          val cleanedDataTypes = summary.dataTypes.intersect(transientDataTypes)
          summary.copy(dataTypes = cleanedDataTypes, dataCount = cleanedDataTypes.size)
      }
      finalizeSummaryWithIds(fbPosts, fbPages, friends, notLikedPagesCount, userSummary, cleanedSummaries)
    }
      ) onFailure {
      case e =>
        log.error(s"Could not reach database : $e")
        analysingFor.remove(userId)
    }
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
        val availableData = availableDataTypes(post)
        val itemSummary = ItemSummary(None, userId, post.id, PostType, availableData, availableData.size)
        itemsSummariesCollection.update(selector, itemSummary, upsert = true)
    }

  }

  /**
    * If there are posts or pages in fbPostsIds and fbPagesIds, use those items to finalize the summary (this time taking
    * into account everything such as post reactioners, number of similar data types to compute the number of order questions
    * etc...) otherwise only aggregates the not read ones in the database (initially the items summaries are stored as not
    * read because they were not aggregated completely with old user summary).
    *
    * @param fbPostsIds  ids of posts to handle
    * @param fbPagesIds  ids of pages to handle
    * @param friends     found friends
    * @param userSummary old user summary
    */
  private def finalizeSummary(fbPostsIds: List[String], fbPagesIds: List[String], friends: Set[Friend], userSummary: UserSummary): Unit = {
    if ((fbPagesIds ++ fbPostsIds).nonEmpty || friends.nonEmpty) {
      val postCollection = db[BSONCollection](MongoCollections.fbPosts)
      val pagesCollection = db[BSONCollection](MongoCollections.fbPages)
      val itemsSummariesCollection = db[BSONCollection](MongoCollections.itemsSummaries)
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
      } yield finalizeSummaryWithIds(fbPosts, fbPages, friends, notLikedPagesCount, userSummary, itemSummaries)
        ) onFailure {
        case e =>
          log.error(s"Could not reach database : $e")
          analysingFor.remove(userId)
      }
    } else {
      log.info(s"There was no final stats to generate (empty lists).")
      analysingFor.remove(userId)
    }
  }

  /**
    * Concretely performs the aggregation
    *
    * @param fbPosts            posts to handle
    * @param fbPages            pages to handle
    * @param friends            found friends
    * @param notLikedPagesCount number of pages not liked
    * @param userSummary        old user summary
    * @param itemSummaries      old items summaries
    */
  private def finalizeSummaryWithIds(fbPosts: List[FBPost], fbPages: List[FBPage], friends: Set[Friend], notLikedPagesCount: Int,
                                     userSummary: UserSummary, itemSummaries: List[ItemSummary]): Unit = {
    val newReactioners = accumulateReactions(fbPosts) ++ userSummary.reactioners
    val blacklist: Set[FBFrom] = userSummary.blacklist.getOrElse(Set())

    val updatedPosts: List[ItemSummary] = updatedPostsSummaries(fbPosts, newReactioners, itemSummaries, blacklist)

    val updatedPages: List[ItemSummary] = updatedPagesSummaries(fbPages, notLikedPagesCount)

    val untouchedPosts = itemSummaries.filterNot(
      is => updatedPosts.exists(ui => ui.userId == is.userId && ui.itemId == is.itemId)
    )

    val newItemsSummaries = updatedPosts ++ untouchedPosts ++ updatedPages

    val itemsSummariesCollection = db[BSONCollection](MongoCollections.itemsSummaries)

    newItemsSummaries.foreach {
      itemSummary =>
        val selector = BSONDocument("userId" -> userId, "itemId" -> itemSummary.itemId)
        itemsSummariesCollection.update(selector, itemSummary, upsert = true)
    }

    val userSummariesCollection = db[BSONCollection](MongoCollections.userSummaries)

    val newUserSummaries = userSummaryWithNewCounts(newReactioners, newItemsSummaries, friends, userSummary)
    val selector = BSONDocument("userId" -> userId)
    userSummariesCollection.update(selector, newUserSummaries, upsert = true)
    analysingFor.remove(userId)
    self ! PoisonPill
  }

  /**
    * Gets reactions from a list of posts
    *
    * @param fbPosts posts to handle
    * @return set of reactions
    */
  private def accumulateReactions(fbPosts: List[FBPost]): Set[AbstractReaction] = {
    fbPosts.foldLeft(Set[AbstractReaction]()) {
      (acc: Set[AbstractReaction], post: FBPost) => {
        acc ++ post.reactions.getOrElse(List()).toSet ++ post.commentsAsReactions
      }
    }
  }

  /**
    * Update post items summaries based on the number of reactioners
    *
    * @param fbPosts       posts to handle
    * @param reactioners   reactioners
    * @param itemSummaries items summaries to update
    * @return list of updated items summaries
    */
  private def updatedPostsSummaries(fbPosts: List[FBPost],
                                   reactioners: Set[AbstractReaction],
                                   itemSummaries: List[ItemSummary],
                                   blacklist: Set[FBFrom] = Set()): List[ItemSummary] = {
    fbPosts.flatMap {
      fbPost =>
        val oldItemSummary = getItemSummary(userId, fbPost.postId, PostType, itemSummaries)
        val reactions = fbPost.reactions.getOrElse(Set()) ++ fbPost.commentsAsReactions
        val filteredPostReactions = applyBlacklist(reactions, blacklist)
        val filteredReactioners = applyBlacklist(reactioners, blacklist)
        val addedTypes = possibleReactions.flatMap(maybeReactionType(filteredPostReactions, filteredReactioners.size)(_))
        val reactionsNumber = filteredPostReactions.size
        if (filteredReactioners.size - reactionsNumber >= 3 && reactionsNumber > 0) {
          val finalListing = (oldItemSummary.dataTypes ++ addedTypes) + PostWhoReacted
          Some(ItemSummary(None, userId, oldItemSummary.itemId, PostType, finalListing, finalListing.size))
        } else {
          if (addedTypes.nonEmpty) {
            val finalListing = oldItemSummary.dataTypes ++ addedTypes
            Some(ItemSummary(None, userId, oldItemSummary.itemId, PostType, finalListing, finalListing.size))
          } else {
            None
          }
        }
    }
  }

  private def maybeReactionType[React <: AbstractReaction](reactions: Set[React], totalReactions: Int)
                                                          (reactionType: ReactionType): Option[ReactionType] = {
    val reactionCount = filterReaction(reactions, reactionType).size
    if (totalReactions - reactionCount >= 3 && reactionCount > 0) {
      Some(reactionType)
    } else {
      None
    }
  }

  /**
    * Updates pages summaries based on the number of not liked pages
    *
    * @param fbPages            pages to handle
    * @param notLikedPagesCount number of not liked pages
    * @return list of items summaries
    */
  private def updatedPagesSummaries(fbPages: List[FBPage], notLikedPagesCount: Int): List[ItemSummary] = {
    val newDataListing =
      if (notLikedPagesCount >= 3) {
        Set[DataType](Time, PageLikeNumber, PageWhichLiked)
      } else {
        Set[DataType](Time, PageLikeNumber)
      }
    fbPages.map {
      fbPage =>
        ItemSummary(None, userId, fbPage.pageId, PageType, newDataListing, newDataListing.size)
    }
  }
}
