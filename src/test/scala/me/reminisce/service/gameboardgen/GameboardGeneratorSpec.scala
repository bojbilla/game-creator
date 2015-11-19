package me.reminisce.service.gameboardgen

import java.util.concurrent.TimeUnit

import akka.testkit.{TestActorRef, TestProbe}
import me.reminisce.database.{DatabaseTestHelper, DatabaseTester}
import me.reminisce.server.domain.Domain.InternalError
import me.reminisce.service.gameboardgen.GameGenerator.CreateBoard
import me.reminisce.service.gameboardgen.GameboardEntities._
import org.scalatest.DoNotDiscover

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.io.Source

@DoNotDiscover
class GameboardGeneratorSpec extends DatabaseTester("GameBoardGeneratorSpec") {
  val testDb = "/anondb"

  "GameboardGenerator" must {
    "generate random boards." in {
      val db = newDb()
      val userId = Source.fromURL(getClass.getResource(testDb + "/userId")).getLines().toList.headOption match {
        case Some(id) =>
          id
        case None =>
          fail("UserId is not defined.")
      }
      DatabaseTestHelper.populateWithTestData(db, testDb)
      val subjects = mutable.Set[Subject]()
      (0 until 10).foreach {
        i =>
          val generator = TestActorRef(GameGenerator.props(db, userId))
          val testProbe = TestProbe()
          testProbe.send(generator, CreateBoard("Wrong_access_token", "chooser"))
          val board = Option(testProbe.receiveOne(Duration(10, TimeUnit.SECONDS)))
          board match {
            case Some(InternalError(message)) =>
              fail(s"Board creation failed with message : $message")
            case Some(Board(id, tiles, isTokenStale)) =>
              assert(id == userId)
              tiles.foreach {
                tile =>
                  addSubjectFromQuestion(subjects, tile.question1)
                  addSubjectFromQuestion(subjects, tile.question2)
                  addSubjectFromQuestion(subjects, tile.question3)
              }
            case any =>
              println(s"Received : $any")
          }
      }
      assert(subjects.size > 120)
    }
  }

  private def addSubjectFromQuestion(subjects: mutable.Set[Subject], question: GameQuestion): Unit = {
    question match {
      case TimelineQuestion(uid, kind, tpe, subject, answer, min, max, default, unit, step, threshold) =>
        subject match {
          case Some(s) => subjects += s
          case None => fail("Subject not defined")
        }
      case MultipleChoiceQuestion(uid, kind, tpe, subject, choices, answer) =>
        subject match {
          case Some(s) => subjects += s
          case None => fail("Subject not defined")
        }
      case GeolocationQuestion(uid, kind, tpe, subject, answer, defaultLocation, range) =>
        subject match {
          case Some(s) => subjects += s
          case None => fail("Subject not defined")
        }
      case OrderQuestion(uid, kind, tpe, subject, choices, answer) =>
        subject match {
          case Some(s) =>
            fail("Order question should not have a subject")
          case None =>
        }

      case _ =>
        fail("Unsupported question type.")
    }
  }

}