package me.reminisce.service.stats

import akka.actor.Props
import me.reminisce.database.DatabaseService
import me.reminisce.fetcher.common.GraphResponses.Post
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.stats.StatsHandler.{FinalStats, TransientPostsStats}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Success}

object StatsHandler {
  val userStatisticsCollection = "user_statistics"
  val postsQuestionsCollection = "posts_questions"
  val fbPostsCollection = "fb_posts"
  val fbPagesCollection = "fb_pages"
  val fbPageLikesCollection = "fb_page_likes"

  case class FinalStats(fbPosts: Set[String])

  case class TransientPostsStats(fbPosts: List[Post])

  def props(user_id: String, db: DefaultDB): Props =
    Props(new StatsHandler(user_id, db))
}

class StatsHandler(user_id: String, db: DefaultDB) extends DatabaseService {

  def receive = {
    case FinalStats(fbPosts) =>
      savePostsStats(fbPosts.toList, db[BSONCollection](StatsHandler.userStatisticsCollection))
    case TransientPostsStats(fbPosts) =>
      saveTransientStats(fbPosts, db[BSONCollection](StatsHandler.postsQuestionsCollection))
    case any =>
      log.error("StatsHandler received unhandled message : " + any)
  }

  def saveTransientStats(fbPosts: List[Post], postQuestionsCollection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    fbPosts.foreach {
      post =>
        val selector = BSONDocument("user_id" -> user_id, "post_id" -> post.id)
        val availableQuestions = availableQuestionTypes(post)
        val postQuestions = PostQuestions(None, user_id, post.id, availableQuestions, availableQuestions.length)
        postQuestionsCollection.update(selector, postQuestions, upsert = true)
    }
  }

  def availableQuestionTypes(post: Post): List[String] = {
    List(checkWhichCoordinatesWereYouAt(post), checkWhichPlaceWereYouAt(post),
      checkWhenDidYouShareThisPost(post), checkWhoMadeThisCommentOnYourPost(post)).flatten
  }

  def checkWhenDidYouShareThisPost(post: Post): Option[String] = {
    if (post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty))
      Some("TLWhenDidYouShareThisPost")
    else
      None
  }

  def checkWhichCoordinatesWereYouAt(post: Post): Option[String] = {
    post.place.flatMap(place => place.location.flatMap(
      location => location.latitude.flatMap(lat => location.longitude.map(
        long => "GeoWhatCoordinatesWereYouAt"
      ))
    ))
  }

  def checkWhichPlaceWereYouAt(post: Post): Option[String] = {
    post.place.flatMap(place => place.name.map(name => "GeoWhichPlaceWereYouAt"))
  }

  def checkWhoMadeThisCommentOnYourPost(post: Post): Option[String] = {
    val fbCommentsCount = post.comments.flatMap { root => root.summary.map { sum => sum.total_count } }
    if ((post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty)) &&
      (post.comments.nonEmpty && fbCommentsCount.exists(_ > 3)))
      Some("MCWhoMadeThisCommentOnYourPost")
    else
      None
  }

  def savePostsStats(newPosts: List[String], userCollection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val postCollection = db[BSONCollection](StatsHandler.fbPostsCollection)
    val selector = BSONDocument("user_id" -> user_id)
    userCollection.find(selector).one[UserStat].onComplete {
      case Success(userStatsOpt) =>
        val userStats = userStatsOpt.fold(UserStat(user_id = user_id))(elm => elm)
        mergeStatsWithNewPosts(postCollection, newPosts, userCollection, userStats)
      case Failure(e) =>
        log.error("Could not access user DB.")
    }
  }

  def mergeStatsWithNewPosts(postCollection: BSONCollection, newPosts: List[String], userCollection: BSONCollection,
                             userStats: UserStat): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val cursor = postCollection.find(BSONDocument("user_id" -> user_id,
      "post_id" -> BSONDocument("$in" -> newPosts))).cursor[FBPost]
    cursor.collect[List](newPosts.length, stopOnError = true).onComplete {
      case Success(list: List[FBPost]) =>
        val newLikers = list.foldLeft(Set[FBLike]()) {
          (acc: Set[FBLike], post: FBPost) => {
            post.likes match {
              case Some(likes) => acc ++ likes.toSet
              case None => acc
            }
          }
        }
        val maxLikerPerPost = (userStats.max_likers_per_post :: list.map(post => post.likes match {
          case Some(likes) => likes.length
          case None => 0
        })).max
        val addedWhoLikedPost = if ((newLikers.size - maxLikerPerPost) >= 3 && !userStats.question_counts.contains("MCWhoLikedYourPost")) {
          addQuestionToMap("MCWhoLikedYourPost", userStats.question_counts)
        }
        else {
          userStats.question_counts
        }
        val postQuestionsCollection = db[BSONCollection](StatsHandler.postsQuestionsCollection)
        val cursor2 = postQuestionsCollection.find(BSONDocument("user_id" -> user_id,
          "post_id" -> BSONDocument("$in" -> newPosts))).cursor[PostQuestions]
        cursor2.collect[List](newPosts.length, stopOnError = true).onComplete {
          case Success(l: List[PostQuestions]) =>
            val questionCounts = l.foldLeft(addedWhoLikedPost) {
              (acc: Map[String, Int], postQuestions: PostQuestions) =>
                addQuestionsToMap(postQuestions.questions, acc)
            }

            val newStats = UserStat(None, user_id, questionCounts, newLikers, maxLikerPerPost)
            val pagesCollection = db[BSONCollection](StatsHandler.fbPagesCollection)
            val pageLikesCollection = db[BSONCollection](StatsHandler.fbPageLikesCollection)
            savePagesStats(pagesCollection, pageLikesCollection, newStats)
          case Failure(e) =>
            log.error("Could not reach database : " + e)
        }
      case _ =>
        log.error("Nothing matched stats request.")
    }
  }

  def savePagesStats(pagesCollection: BSONCollection, pageLikesCollection: BSONCollection, userStat: UserStat): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val selector = BSONDocument(
      "user_id" -> user_id
    )
    val likedPageIds = pageLikesCollection.find(selector).cursor[FBPageLike].collect[List]().map {
      likes =>
        likes.map {
          like =>
            like.page_id
        }
    }
    likedPageIds.onComplete {
      case Success(pIds) =>
        val queryUnliked = BSONDocument(
          "page_id" -> BSONDocument("$nin" -> pIds)
        )
        pagesCollection.find(queryUnliked).cursor[FBPage].collect[List](3).onComplete {
          case Success(unlikedPages) =>
            val newUserStat = {
              if (unlikedPages.length >= 3) {
                val oldQuestionCounts = userStat.question_counts
                val newQuestionCounts = {
                  if (oldQuestionCounts.contains("MCWhichPageDidYouLike"))
                    oldQuestionCounts
                  else
                    oldQuestionCounts + ("MCWhichPageDidYouLike" -> 1)
                }
                UserStat(None, userStat.user_id, newQuestionCounts)
              } else {
                userStat
              }
            }
            val userCollection = db[BSONCollection](StatsHandler.userStatisticsCollection)
            userCollection.update(selector, newUserStat, upsert = true)
          case Failure(e) =>
            log.error("Wow, strangeness !" + e)
        }
      case Failure(e) =>
        log.error("Wow, strangeness !" + e)
    }
  }

  def addQuestionToMap(question: String, oldMap: Map[String, Int]): Map[String, Int] = {
    if (oldMap.contains(question)) {
      oldMap.updated(question, oldMap(question) + 1)
    } else {
      oldMap.updated(question, 1)
    }
  }

  def addQuestionsToMap(questions: List[String], oldMap: Map[String, Int]): Map[String, Int] = {
    questions match {
      case Nil => oldMap
      case x :: xs => addQuestionsToMap(xs, addQuestionToMap(x, oldMap))
    }
  }
}
