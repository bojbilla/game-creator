package me.reminisce.gameboard.questions

import com.github.nscala_time.time.Imports._
import me.reminisce.gameboard.board.GameboardEntities._

import scala.util.Random

/**
  * Utility methods to generate time questions
  */
object TimeQuestionGenerator {
  /**
    * Each position in the array corresponds to an element in [[org.joda.time.Period]] getValues array, this is used to
    * convert to our custom time units
    */
  val periodValuesToUnits = Array(Year, Month, Week, Day)

  /**
    * Given a date in the past generate a range used for a Timeline question
    *
    * @param actualDate date in the past
    * @return a (min, default, max, unit, step) tuple used to generate a Timeline question
    */
  def generateRange(actualDate: DateTime): (DateTime, DateTime, DateTime, TimeUnit, Int) = {
    val currentTime = DateTime.now
    val step = new Period(actualDate.getMillis, actualDate.getMillis + (currentTime.getMillis - actualDate.getMillis) / QuestionGenerationConfig.timelineStepsNumber)
    step.getValues.zip(periodValuesToUnits).find { case (stepSize, unit) => stepSize > 0 } match {
      case Some((stepSize, unit)) =>
        val maxNumber = Random.nextInt(QuestionGenerationConfig.timelineStepsNumber + 1)
        val defaultNumber = Random.nextInt(QuestionGenerationConfig.timelineStepsNumber + 1)
        val minNumber = QuestionGenerationConfig.timelineStepsNumber - maxNumber
        val period = unitToPeriod(unit)
        val min = actualDate - period.multipliedBy(stepSize).multipliedBy(minNumber)
        val default = min + period.multipliedBy(stepSize).multipliedBy(defaultNumber)
        val max = actualDate + period.multipliedBy(stepSize).multipliedBy(maxNumber)
        (min, default, max, unit, stepSize)
      case None =>
        val min = actualDate - 1.day.multipliedBy(QuestionGenerationConfig.timelineStepsNumber)
        val defaultNumber = Random.nextInt(QuestionGenerationConfig.timelineStepsNumber + 1)
        val default = min + 1.day.multipliedBy(defaultNumber)
        (min, default, actualDate, Day, 1)
    }
  }

  /**
    * Converts our time units to an equivalent period
    *
    * @param unit unit to convert
    * @return a period which lasts one unit
    */
  def unitToPeriod(unit: TimeUnit): Period = unit match {
    case Year =>
      1.year
    case Month =>
      1.month
    case Week =>
      1.week
    case _ =>
      1.day
  }

  /**
   * Change the step size according to the difficulty
   * 
   * @param difficulty The difficulty
   * @param step The step size
	 */
  def stepWithDifficulty(difficulty: Option[Double], step: Int): Int = {
    difficulty match {
      case None => step
      case Some(d) => {
        Math.max(1, Math.rint(step * d)).toInt
      }
    }
  }
}

  
/**
  * Abstract time question generator
  */
abstract class TimeQuestionGenerator extends QuestionGenerator
