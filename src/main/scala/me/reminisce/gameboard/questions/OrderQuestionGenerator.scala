package me.reminisce.gameboard.questions

import me.reminisce.gameboard.board.GameboardEntities.{Subject, SubjectWithId}

import scala.util.Random

/**
  * Utility methods for an order question generator
  */
object OrderQuestionGenerator {

  /**
    * Given a list of subjects, associates them with ids and shuffles them
    * @param orderedSubjects subjects to associate with ids
    * @return a list of shuffled subject with ids and the right order of ids
    */
  def generateSubjectsWithId(orderedSubjects: List[Subject]): (List[SubjectWithId], List[Int]) = {
    val answer = Random.shuffle(orderedSubjects.indices.toList)
    val subjectsWithId = Random.shuffle(orderedSubjects.zip(answer).map {
      case (subject, id) => SubjectWithId(subject, id)
    })
    (subjectsWithId, answer)
  }

}

/**
  * Abstract order question generator.
  */
abstract class OrderQuestionGenerator extends QuestionGenerator {

  lazy val itemsToOrder = QuestionGenerationConfig.orderingItemsNumber

}
