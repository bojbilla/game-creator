package me.reminisce.gameboard.questions

import me.reminisce.gameboard.board.GameboardEntities.{SubjectWithId, TextPostSubject}
import org.scalatest.FunSuite

class OrderQuestionGeneratorSuite extends FunSuite {

  test("Empty list of subjects to SubjectWithIds.") {
    assert(OrderQuestionGenerator.generateSubjectsWithId(List()) == (List(), List()))
  }

  test("List of subjects to SubjectWithIds.") {
    val subject1 = TextPostSubject("Salmon.", from = None)
    val subject2 = TextPostSubject("Banana.", from = None)
    val subject3 = TextPostSubject("Orange.", from = None)
    val subject4 = TextPostSubject("Tuna.", from = None)

    val listOfSubjects = List(subject1, subject2, subject3, subject4)

    val propositions = OrderQuestionGenerator.generateSubjectsWithId(listOfSubjects)

    propositions match {
      case (subjectWithIds, List(answer1, answer2, answer3, answer4)) =>
        /*
        * As subject1, subject2, subject3, subject4 was a list of ordered subjects, then subject1 must be associated
        * with the first item in answers, subject2 with the second one, subject3 with the third one and subject 4 with
        * the fourth one.
        */
        assert(subjectWithIds.contains(SubjectWithId(subject1, answer1)))
        assert(subjectWithIds.contains(SubjectWithId(subject2, answer2)))
        assert(subjectWithIds.contains(SubjectWithId(subject3, answer3)))
        assert(subjectWithIds.contains(SubjectWithId(subject4, answer4)))
      case _ =>
        fail("Not enough answers.")
    }
  }
}
