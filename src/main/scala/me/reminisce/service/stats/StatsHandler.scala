package me.reminisce.service.stats

import akka.actor.Props
import me.reminisce.database.{DatabaseService, MongoDatabaseService}
import me.reminisce.fetcher.common.GraphResponses.Post
import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.stats.StatsHandler.{FinalStats, TransientPostsStats}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Success}

object StatsHandler {

  case class FinalStats(fbPosts: Set[String])

  case class TransientPostsStats(fbPosts: List[Post])

  def props(userId: String, db: DefaultDB): Props =
    Props(new StatsHandler(userId, db))
}

class StatsHandler(userId: String, db: DefaultDB) extends DatabaseService {

  def receive = {
    case FinalStats(fbPosts) =>
      savePostsStats(fbPosts.toList, db[BSONCollection](MongoDatabaseService.userStatisticsCollection))
    case TransientPostsStats(fbPosts) =>
      saveTransientStats(fbPosts, db[BSONCollection](MongoDatabaseService.postQuestionsCollection))
    case any =>
      log.error("StatsHandler received unhandled message : " + any)
  }

  def saveTransientStats(fbPosts: List[Post], postQuestionsCollection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val userCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)
    val selector = BSONDocument("userId" -> userId)
    userCollection.find(selector).one[UserStats].onComplete {
      case Success(userStatsOpt) =>
        val userStats = userStatsOpt.fold(UserStats(userId = userId))(elm => elm)
        fbPosts.foreach {
          post =>
            val selector = BSONDocument("userId" -> userId, "postId" -> post.id)
            val availableQuestions = availableQuestionTypes(post, userStats)
            val postQuestions = PostQuestions(None, userId, post.id, availableQuestions, availableQuestions.length)
            postQuestionsCollection.update(selector, postQuestions, upsert = true)
        }
      case Failure(e) =>
        log.error("Could not reach database : " + e)
    }
  }

  def availableQuestionTypes(post: Post, userStats: UserStats): List[String] = {
    List(checkWhichCoordinatesWereYouAt(post),
      checkWhenDidYouShareThisPost(post), checkWhoMadeThisCommentOnYourPost(post),
      checkWhoLikedYourPost(post, userStats)).flatten
  }

  def checkWhoLikedYourPost(post: Post, userStats: UserStats): Option[String] = {
    post.likes.flatMap {
      list => list.data.flatMap {
        data =>
          if (userStats.likers.size - data.length >= 3) {
            Some("MCWhoLikedYourPost")
          } else {
            None
          }
      }
    }
  }

  def checkWhenDidYouShareThisPost(post: Post): Option[String] = {
    if ((post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty)) && post.created_time.nonEmpty)
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

  def checkWhoMadeThisCommentOnYourPost(post: Post): Option[String] = {
    val fbComment = post.comments.flatMap(root => root.data.map(comments => comments.map { c =>
      FBComment(c.id, FBFrom(c.from.id, c.from.name), c.like_count, c.message)
    }))
    if ((post.message.exists(!_.isEmpty) || post.story.exists(!_.isEmpty)) && fbComment.nonEmpty) {
      val fromSet = fbComment.get.map {
        comm => comm.from
      }.toSet
      if (fromSet.size > 3) {
        Some("MCWhoMadeThisCommentOnYourPost")
      } else {
        None
      }
    } else {
      None
    }
  }

  def savePostsStats(newPosts: List[String], userCollection: BSONCollection): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
    val selector = BSONDocument("userId" -> userId)
    userCollection.find(selector).one[UserStats].onComplete {
      case Success(userStatsOpt) =>
        val userStats = userStatsOpt.fold(UserStats(userId = userId))(elm => elm)
        mergeStatsWithNewPosts(postCollection, newPosts, userCollection, userStats)
      case Failure(e) =>
        log.error("Could not access user DB.")
    }
  }


  def mergeStatsWithNewPosts(postCollection: BSONCollection, newPosts: List[String], userCollection: BSONCollection,
                             userStats: UserStats): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val cursor = postCollection.find(BSONDocument("userId" -> userId,
      "postId" -> BSONDocument("$in" -> newPosts))).cursor[FBPost]
    cursor.collect[List](newPosts.length, stopOnError = true).onComplete {
      case Success(list: List[FBPost]) =>
        val newLikers = list.foldLeft(Set[FBLike]()) {
          (acc: Set[FBLike], post: FBPost) => {
            post.likes match {
              case Some(likes) => acc ++ likes.toSet
              case None => acc
            }
          }
        } ++ userStats.likers
        val maxLikerPerPost = (userStats.maxLikerPerPost :: list.map(post => post.likes match {
          case Some(likes) => likes.length
          case None => 0
        })).max

        val postQuestionsCollection = db[BSONCollection](MongoDatabaseService.postQuestionsCollection)
        val cursor2 = postQuestionsCollection.find(BSONDocument("userId" -> userId,
          "postId" -> BSONDocument("$in" -> newPosts))).cursor[PostQuestions]
        cursor2.collect[List](newPosts.length, stopOnError = true).onComplete {
          case Success(l: List[PostQuestions]) =>
            val questionCounts = l.foldLeft(userStats.questionCounts) {
              (acc: Map[String, Int], postQuestions: PostQuestions) =>
                addQuestionsToMap(postQuestions.questions, acc)
            }

            val newStats = UserStats(None, userId, questionCounts, newLikers, maxLikerPerPost)
            if ((newLikers.size - maxLikerPerPost) >= 3 && !userStats.questionCounts.contains("MCWhoLikedYourPost")) {
              updateWhoLikedYourPost(userCollection, postCollection, newStats)
            } else {
              val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
              val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
              savePagesStats(pagesCollection, pageLikesCollection, newStats)
            }
          case Failure(e) =>
            log.error("Could not reach database : " + e)
        }
      case _ =>
        log.error("Nothing matched stats request.")
    }
  }

  def savePagesStats(pagesCollection: BSONCollection, pageLikesCollection: BSONCollection, userStats: UserStats): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val selector = BSONDocument(
      "userId" -> userId
    )
    val likedPageIds = pageLikesCollection.find(selector).cursor[FBPageLike].collect[List]().map {
      likes =>
        likes.map {
          like =>
            like.pageId
        }
    }
    likedPageIds.onComplete {
      case Success(pIds) =>
        val queryNotLiked = BSONDocument(
          "pageId" -> BSONDocument("$nin" -> pIds)
        )
        pagesCollection.find(queryNotLiked).cursor[FBPage].collect[List](3).onComplete {
          case Success(notLikedPages) =>
            val newUserStats = {
              if (notLikedPages.length >= 3) {
                val oldQuestionCounts = userStats.questionCounts
                val newQuestionCounts = oldQuestionCounts.updated("MCWhichPageDidYouLike", pIds.length)
                UserStats(None, userStats.userId, newQuestionCounts, userStats.likers, userStats.maxLikerPerPost)
              } else {
                userStats
              }
            }
            val userCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)
            userCollection.update(selector, newUserStats, upsert = true)
          case Failure(e) =>
            log.error("Could not reach database : " + e)
        }
      case Failure(e) =>
        log.error("Could not reach database : " + e)
    }
  }

  //TODO: make this better
  def updateWhoLikedYourPost(userCollection: BSONCollection, postCollection: BSONCollection, userStats: UserStats): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val selector = BSONDocument("userId" -> userId)
    postCollection.find(selector).cursor[FBPost].collect[List]().onComplete {
      case Success(list) =>
        val interestingPosts = list.map {
          post => post.postId -> post.likes.fold(0)(elm => elm.length)
        }.filter(elm => elm._2 > 0 && userStats.likers.size - elm._2 >= 3).map(elm => elm._1)
        val postQuestionsCollection = db[BSONCollection](MongoDatabaseService.postQuestionsCollection)
        val postQuestionsSelector = BSONDocument("userId" -> userId,
          "postId" -> BSONDocument("$in" -> interestingPosts))
        postQuestionsCollection.find(postQuestionsSelector).cursor[PostQuestions].collect[List]().onComplete {
          case Success(postQuestionsList) =>
            val newQuestions = postQuestionsList.flatMap {
              postQuestions =>
                if (!postQuestions.questions.contains("MCWhoLikedYourPost")) {
                  val newPostQuestions = PostQuestions(None, userId, postQuestions.postId,
                    "MCWhoLikedYourPost" :: postQuestions.questions, postQuestions.questionsCount + 1)
                  val selector = BSONDocument("userId" -> userId, "postId" -> postQuestions.postId)
                  postQuestionsCollection.update(selector, newPostQuestions, upsert = true)
                  Some("MCWhoLikedYourPost")
                } else {
                  None
                }
            }
            val newUserStats = UserStats(None, userId, addQuestionsToMap(newQuestions, userStats.questionCounts),
              userStats.likers, userStats.maxLikerPerPost)
            val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
            val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
            savePagesStats(pagesCollection, pageLikesCollection, newUserStats)
          case Failure(e) =>
            log.error("Could not reach database : " + e)
        }
      case Failure(e) =>
        log.error("Could not reach database : " + e)

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
