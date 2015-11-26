package me.reminisce.service.gameboardgen.questiongen

import com.github.nscala_time.time.Imports._
import me.reminisce.service.gameboardgen.GameboardEntities.TimeUnit
import me.reminisce.service.gameboardgen.GameboardEntities.TimeUnit.TimeUnit
import org.joda.time.{Days, Months, Weeks, Years}

import scala.util.Random

object TimeQuestionGenerator {
  def generateRange(actualDate: DateTime): (DateTime, DateTime, TimeUnit) = {
    val currentTime = DateTime.now
    val yearsDiff = Years.yearsBetween(actualDate, currentTime).getYears
    val monthsDiff = Months.monthsBetween(actualDate, currentTime).getMonths
    val weeksDiff = Weeks.weeksBetween(actualDate, currentTime).getWeeks
    val daysDiff = Days.daysBetween(actualDate, currentTime).getDays


    val params = if (yearsDiff > 0) {
      (1.year, yearsDiff, TimeUnit.Year)
    } else if (monthsDiff > 0) {
      (1.month, monthsDiff, TimeUnit.Month)
    } else if (weeksDiff > 0) {
      (1.week, weeksDiff, TimeUnit.Week)
    } else {
      (1.day, daysDiff, TimeUnit.Day)
    }
    params match {
      case (stepSize, difference, unit) =>
        //difference because for years, months, weeks etc... it can never be 0 ago
        val maxStepsForward = List[Int](difference - 1, 4).min
        val stepsForward = if (maxStepsForward > 0) Random.nextInt(maxStepsForward) else 0

        val min = actualDate - stepSize.multipliedBy(4 - stepsForward)
        val max = actualDate + stepSize.multipliedBy(stepsForward)
        (min, max, unit)
    }
  }
}

abstract class TimeQuestionGenerator extends QuestionGenerator
