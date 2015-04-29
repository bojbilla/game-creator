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

  def props(user_id: String, db: DefaultDB): Props =
    Props(new StatsHandler(user_id, db))
}

class StatsHandler(user_id: String, db: DefaultDB) extends DatabaseService {

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
    val selector = BSONDocument("user_id" -> user_id)
    userCollection.find(selector).one[UserStat].onComplete {
      case Success(userStatOpt) =>
        val userStat = userStatOpt.fold(UserStat(user_id = user_id))(elm => elm)
        fbPosts.foreach {
          post =>
            val selector = BSONDocument("user_id" -> user_id, "post_id" -> post.id)
            val availableQuestions = availableQuestionTypes(post, userStat)
            val postQuestions = PostQuestions(None, user_id, post.id, availableQuestions, availableQuestions.length)
            postQuestionsCollection.update(selector, postQuestions, upsert = true)
        }
      case Failure(e) =>
        log.error("Could not reach database : " + e)
    }
  }

  def availableQuestionTypes(post: Post, userStat: UserStat): List[String] = {
    List(checkWhichCoordinatesWereYouAt(post), checkWhichPlaceWereYouAt(post),
      checkWhenDidYouShareThisPost(post), checkWhoMadeThisCommentOnYourPost(post),
      checkWhoLikedYourPost(post, userStat)).flatten
  }

  def checkWhoLikedYourPost(post: Post, userStat: UserStat): Option[String] = {
    post.likes.flatMap {
      list => list.data.flatMap {
        data =>
          if (userStat.likers.size - data.length >= 3) {
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

  def checkWhichPlaceWereYouAt(post: Post): Option[String] = {
    post.place.flatMap(place => place.name.map(name => "GeoWhichPlaceWereYouAt"))
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

        val postQuestionsCollection = db[BSONCollection](MongoDatabaseService.postQuestionsCollection)
        val cursor2 = postQuestionsCollection.find(BSONDocument("user_id" -> user_id,
          "post_id" -> BSONDocument("$in" -> newPosts))).cursor[PostQuestions]
        cursor2.collect[List](newPosts.length, stopOnError = true).onComplete {
          case Success(l: List[PostQuestions]) =>
            val questionCounts = l.foldLeft(userStats.question_counts) {
              (acc: Map[String, Int], postQuestions: PostQuestions) =>
                addQuestionsToMap(postQuestions.questions, acc)
            }

            val newStats = UserStat(None, user_id, questionCounts, newLikers, maxLikerPerPost)
            if ((newLikers.size - maxLikerPerPost) >= 3 && !userStats.question_counts.contains("MCWhoLikedYourPost")) {
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
        val queryNotLiked = BSONDocument(
          "page_id" -> BSONDocument("$nin" -> pIds)
        )
        pagesCollection.find(queryNotLiked).cursor[FBPage].collect[List](3).onComplete {
          case Success(notLikedPages) =>
            val newUserStat = {
              if (notLikedPages.length >= 3) {
                val oldQuestionCounts = userStat.question_counts
                val newQuestionCounts = oldQuestionCounts.updated("MCWhichPageDidYouLike", pIds.length)
                UserStat(None, userStat.user_id, newQuestionCounts, userStat.likers, userStat.max_likers_per_post)
              } else {
                userStat
              }
            }
            val userCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)
            userCollection.update(selector, newUserStat, upsert = true)
          case Failure(e) =>
            log.error("Could not reach database : " + e)
        }
      case Failure(e) =>
        log.error("Could not reach database : " + e)
    }
  }

  //TODO: make this better
  def updateWhoLikedYourPost(userCollection: BSONCollection, postCollection: BSONCollection, userStat: UserStat): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val selector = BSONDocument("user_id" -> user_id)
    postCollection.find(selector).cursor[FBPost].collect[List]().onComplete {
      case Success(list) =>
        val interestingPosts = list.map {
          post => post.post_id -> post.likes.fold(0)(elm => elm.length)
        }.filter(elm => elm._2 > 0 && userStat.likers.size - elm._2 >= 3).map(elm => elm._1)
        val postQuestionsCollection = db[BSONCollection](MongoDatabaseService.postQuestionsCollection)
        val postQuestionsSelector = BSONDocument("user_id" -> user_id,
          "post_id" -> BSONDocument("$in" -> interestingPosts))
        postQuestionsCollection.find(postQuestionsSelector).cursor[PostQuestions].collect[List]().onComplete {
          case Success(postQuestionsList) =>
            val newQuestions = postQuestionsList.flatMap {
              postQuestions =>
                if (!postQuestions.questions.contains("MCWhoLikedYourPost")) {
                  val newPostQuestions = PostQuestions(None, user_id, postQuestions.post_id,
                    "MCWhoLikedYourPost" :: postQuestions.questions, postQuestions.questions_count + 1)
                  val selector = BSONDocument("user_id" -> user_id, "post_id" -> postQuestions.post_id)
                  postQuestionsCollection.update(selector, newPostQuestions, upsert = true)
                  Some("MCWhoLikedYourPost")
                } else {
                  None
                }
            }
            val newUserStat = UserStat(None, user_id, addQuestionsToMap(newQuestions, userStat.question_counts),
              userStat.likers, userStat.max_likers_per_post)
            val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
            val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
            savePagesStats(pagesCollection, pageLikesCollection, newUserStat)
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
