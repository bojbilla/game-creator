package me.reminisce.gameboard.questions

import java.util.concurrent.TimeUnit

import akka.testkit.TestProbe
import me.reminisce.database.DatabaseTester
import me.reminisce.gameboard.board.GameboardEntities
import me.reminisce.gameboard.board.GameboardEntities.{GameQuestion, OrderQuestion, Subject}
import me.reminisce.gameboard.questions.QuestionGenerator.FinishedQuestionCreation

import scala.concurrent.duration.Duration
import scala.reflect.{ClassTag, classTag}

abstract class QuestionTester(actorSystemName: String) extends DatabaseTester(actorSystemName) {

  protected def checkOpt[T](opt: Option[T])(check: T => Unit)(fail: => Unit): Unit = {
    opt match {
      case Some(t) =>
        check(t)
      case None =>
        fail
    }
  }

  protected def checkFinished[qType <: GameQuestion : ClassTag](testProbe: TestProbe)(check: qType => Unit): Unit = {
    checkOpt(Option(testProbe.receiveOne(Duration(10, TimeUnit.SECONDS)))) {
      finishedCreation =>
        assert(finishedCreation.isInstanceOf[FinishedQuestionCreation])

        val question = finishedCreation.asInstanceOf[FinishedQuestionCreation].question
        assert(classTag[qType].runtimeClass.isInstance(question))

        check(question.asInstanceOf[qType])
    } {
      fail("Did not receive feedback.")
    }
  }

  protected def orderCheck[sType <: Subject : ClassTag](question: OrderQuestion)(check: (sType, Int) => Unit): Unit = {
    val subjectWithIds = question.choices
    val answer = question.answer
    answer.indices.foreach {
      case nb =>
        val a = answer(nb)
        checkOpt(subjectWithIds.find(elm => elm.uId == a)) {
          subjectWithId =>
            val subject = subjectWithId.subject
            assert(classTag[sType].runtimeClass.isInstance(subject))
            check(subject.asInstanceOf[sType], nb)
        } {
          fail(s"No subject corresponds to answer $a.")
        }
    }
  }

  protected def checkSubject[sType <: Subject : ClassTag](sOpt: Option[Subject])(check: sType => Unit): Unit = {
    checkOpt(sOpt) {
      s =>
        assert(classTag[sType].runtimeClass.isInstance(s))
        check(s.asInstanceOf[sType])
    } {
      fail("Subject is not defined.")
    }
  }

}
