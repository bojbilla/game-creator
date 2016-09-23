package me.reminisce.testutils

import java.util.concurrent.TimeUnit

import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/**
  * Contains methods to retrieve entities from a mongo collection. They will retry until something is found which matches
  * the check. If the check trivially returns true, it still ensures that something was found.
  */
object Retry {
  private val attemptsPermitted = 20

  def find[T](collection: BSONCollection, selector: BSONDocument, attempts: Int)
             (check: T => Boolean)(implicit reader: BSONDocumentReader[T]): Option[T] = {
    Await.result[Option[T]](collection.find(selector).one[T], Duration(10, TimeUnit.SECONDS)) match {
      case Some(result) =>
        if (check(result)) {
          Some(result)
        } else {
          attempt[Option[T]](attempts, None) {
            find(collection, selector, attempts + 1)(check)
          }
        }
      case None =>
        attempt[Option[T]](attempts, None) {
          find(collection, selector, attempts + 1)(check)
        }
    }
  }

  private def attempt[T](attempts: Int, default: T)(block: => T): T = {
    if (attempts < attemptsPermitted) {
      Thread.sleep(200)
      block
    } else {
      default
    }
  }

  def findList[T](collection: BSONCollection, selector: BSONDocument, attempts: Int)
                 (check: List[T] => Boolean)(implicit reader: BSONDocumentReader[T]): List[T] = {
    Await.result(collection.find(selector).cursor[T]().collect[List](stopOnError = true),
      Duration(10, TimeUnit.SECONDS)) match {
      case List() =>
        attempt[List[T]](attempts, Nil) {
          findList(collection, selector, attempts + 1)(check)
        }
      case result =>
        if (check(result)) {
          result
        } else {
          attempt[List[T]](attempts, Nil) {
            findList(collection, selector, attempts + 1)(check)
          }
        }
    }
  }
}
