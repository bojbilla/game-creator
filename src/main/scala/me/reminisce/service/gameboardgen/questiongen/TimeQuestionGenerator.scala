package me.reminisce.service.gameboardgen.questiongen

import com.github.nscala_time.time.Imports._
import me.reminisce.service.gameboardgen.GameboardEntities.TimeUnit
import me.reminisce.service.gameboardgen.GameboardEntities.TimeUnit.TimeUnit

import scala.util.Random

object TimeQuestionGenerator {
  val periodValuesToUnits = Array(TimeUnit.Year, TimeUnit.Month, TimeUnit.Week, TimeUnit.Day)

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
        (min, default, actualDate, TimeUnit.Day, 1)
    }
  }

  def unitToPeriod(unit: TimeUnit): Period = unit match {
    case TimeUnit.Year =>
      1.year
    case TimeUnit.Month =>
      1.month
    case TimeUnit.Week =>
      1.week
    case _ =>
      1.day
  }

}

abstract class TimeQuestionGenerator extends QuestionGenerator
