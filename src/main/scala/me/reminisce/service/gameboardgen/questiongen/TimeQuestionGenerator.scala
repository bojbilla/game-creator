package me.reminisce.service.gameboardgen.questiongen

import com.github.nscala_time.time.Imports._
import me.reminisce.service.gameboardgen.GameboardEntities.TimeUnit
import me.reminisce.service.gameboardgen.GameboardEntities.TimeUnit.TimeUnit
import org.joda.time.{Days, Months, Weeks, Years}

import scala.util.Random

abstract class TimeQuestionGenerator extends QuestionGenerator {
  def generateRange(actualDate: DateTime): (DateTime, DateTime, TimeUnit) = {
    val currentTime = DateTime.now
    val yearsDiff = Years.yearsBetween(actualDate, currentTime).getYears
    val monthsDiff = Months.monthsBetween(actualDate, currentTime).getMonths
    val weeksDiff = Weeks.weeksBetween(actualDate, currentTime).getWeeks
    val daysDiff = Days.daysBetween(actualDate, currentTime).getDays

    val (stepSize, difference) =
      if (yearsDiff > 0) {
        (1.year, yearsDiff)
      } else if (monthsDiff > 0) {
        (1.month, monthsDiff)
      } else if (weeksDiff > 0) {
        (1.week, weeksDiff)
      } else {
        (1.day, daysDiff)
      }

    val maxStepsForward = List[Int](difference, 4).min
    val stepsForward = if (maxStepsForward > 0) Random.nextInt(maxStepsForward) else maxStepsForward

    val min = actualDate - stepSize.multipliedBy(4 - stepsForward)
    val max = actualDate + stepSize.multipliedBy(stepsForward)
    (min, max, periodToTimeUnit(stepSize))
  }

  def periodToTimeUnit(period: Period): TimeUnit = {
    if (period == 1.year) {
      TimeUnit.Year
    } else if (period == 1.month) {
      TimeUnit.Month
    } else if (period == 1.week) {
      TimeUnit.Week
    } else {
      TimeUnit.Day
    }
  }
}
