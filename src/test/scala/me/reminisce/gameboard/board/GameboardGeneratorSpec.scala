package me.reminisce.gameboard.board

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.DatabaseFiller
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.{FBFrom, LastFetched}
import me.reminisce.gameboard.board.GameGenerator.CreateBoard
import me.reminisce.gameboard.board.GameboardEntities.SubjectType.SubjectType
import me.reminisce.gameboard.board.GameboardEntities._
import me.reminisce.server.domain.Domain.InternalError
import me.reminisce.testutils.database.DatabaseTester
import org.joda.time.DateTime
import org.scalatest.DoNotDiscover
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteConcern

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source

@DoNotDiscover
class GameboardGeneratorSpec extends DatabaseTester("GameBoardGeneratorSpec") {
  val testDb = "/anondb"

  "GameboardGenerator" must {
    "generate valid boards." in {
      testWithDb {
        db =>
          val userId = Source.fromURL(getClass.getResource(testDb + "/userId")).getLines().toList.headOption match {
            case Some(id) =>
              id
            case None =>
              fail("UserId is not defined.")
          }
          DatabaseFiller.populateWithTestData(db, testDb)
          val lastFetched = LastFetched(None, userId, DateTime.now)
          db[BSONCollection](MongoCollections.lastFetched).update(lastFetched, lastFetched, WriteConcern.Acknowledged, upsert = true)
          (0 until 10).foreach {
            i =>
              val generator = TestActorRef(GameGenerator.props(db, userId))
              val testProbe = TestProbe()
              testProbe.send(generator, CreateBoard("Wrong_access_token", "chooser"))
              val board = Option(testProbe.receiveOne(Duration(10, TimeUnit.SECONDS)))
              board match {
                case Some(InternalError(message)) =>
                  fail(s"Board creation failed with message : $message")
                case Some(Board(id, tiles, isTokenStale, strategy)) =>
                  assert(id == userId)
                  tiles.foreach {
                    tile =>
                      validateQuestion(tile.question1)
                      validateQuestion(tile.question2)
                      validateQuestion(tile.question3)
                  }
                case any =>
                  println(s"Received : $any")
              }
          }
      }
    }
  }

  def validateSubject(subject: Subject): Unit = {
    subject match {
      case PageSubject(name: String, pageId: String, photoUrl: Option[String], tpe: SubjectType) =>
        assert(tpe == SubjectType.PageSubject)
        assert(name.nonEmpty)
        assert(pageId.nonEmpty)
      case TextPostSubject(text: String, tpe: SubjectType, from: Option[FBFrom]) =>
        assert(tpe == SubjectType.TextPost)
        assert(text.nonEmpty)
      case ImagePostSubject(text: String, imageUrl: Option[String], facebookImageUrl: Option[String], tpe: SubjectType, from: Option[FBFrom]) =>
        assert(tpe == SubjectType.ImagePost)
        assert(text.nonEmpty)
      case VideoPostSubject(text: String, thumbnailUrl: Option[String], url: Option[String], tpe: SubjectType, from: Option[FBFrom]) =>
        assert(tpe == SubjectType.VideoPost)
        assert(text.nonEmpty)
      case LinkPostSubject(text: String, thumbnailUrl: Option[String], url: Option[String], tpe: SubjectType, from: Option[FBFrom]) =>
        assert(tpe == SubjectType.LinkPost)
        assert(text.nonEmpty)
      case CommentSubject(comment: String, post: PostSubject, tpe: SubjectType) =>
        assert(tpe == SubjectType.CommentSubject)
        assert(comment.nonEmpty)
        validateSubject(post)
      case _ =>
        fail("Non supported subject type.")
    }
  }

  /**
    * Validates the question, for the moment it only verifies that the subject is valid
    *
    * @param question question to validate
    */
  private def validateQuestion(question: GameQuestion): Unit = {

    question match {
      case TimelineQuestion(uid, kind, tpe, subject, answer, min, max, default, unit, step, threshold) =>
        subject match {
          case Some(s) => validateSubject(s)
          case None => fail("Subject not defined")
        }
      case MultipleChoiceQuestion(uid, kind, tpe, subject, choices, answer) =>
        subject match {
          case Some(s) => validateSubject(s)
          case None => fail("Subject not defined")
        }
      case GeolocationQuestion(uid, kind, tpe, subject, answer, defaultLocation, range) =>
        subject match {
          case Some(s) => validateSubject(s)
          case None => fail("Subject not defined")
        }
      case OrderQuestion(uid, kind, tpe, subject, choices, answer) =>
        subject match {
          case Some(s) => validateSubject(s)
            fail("Order question should not have a subject")
          case None =>
        }

      case _ =>
        fail("Unsupported question type.")
    }
  }

}