package me.reminisce.service.gameboardgen.questiongen

import me.reminisce.service.gameboardgen.GameboardEntities.{Subject, SubjectWithId}

import scala.util.Random

abstract class OrderQuestionGenerator extends QuestionGenerator {

  lazy val itemsToOrder = QuestionGenerationConfig.orderingItemsNumber

  def generateSubjectsWithId(orderedSubjects: List[Subject]): (List[SubjectWithId], List[Int]) = {
    val answer = Random.shuffle((0 until orderedSubjects.size).toList)
    val subjectsWithId = Random.shuffle(orderedSubjects.zip(answer).map {
      case (subject, id) => SubjectWithId(subject, id)
    })
    (subjectsWithId, answer)
  }

}
