package me.reminisce.service.gameboardgen.questiongen

import com.github.nscala_time.time.Imports._
import me.reminisce.service.gameboardgen.GameboardEntities.TimeUnit
import org.scalatest.FunSuite

class TimeQuestionGeneratorSuite extends FunSuite {
  test("Testing range generation.") {
    val now = DateTime.now
    val oneYearAgo = now - 1.year
    val twoYearsAgo = now - 2.year
    val twoMonthsAgo = now - 2.month
    val threeWeeksAgo = now - 3.week
    val fourDaysAgo = now - 4.day

    TimeQuestionGenerator.generateRange(oneYearAgo) match {
      case (minOneYearAgo, maxOneYearAgo, unitOneYearAgo) =>
        assert(minOneYearAgo <= oneYearAgo)
        assert(maxOneYearAgo >= oneYearAgo)
        assert(unitOneYearAgo == TimeUnit.Year)
    }

    TimeQuestionGenerator.generateRange(twoYearsAgo) match {
      case (minTwoYearsAgo, maxTwoYearsAgo, unitTwoYearsAgo) =>
        assert(minTwoYearsAgo <= twoYearsAgo)
        assert(maxTwoYearsAgo >= twoYearsAgo)
        assert(unitTwoYearsAgo == TimeUnit.Year)
    }

    TimeQuestionGenerator.generateRange(twoMonthsAgo) match {
      case (minTwoMonthsAgo, maxTwoMonthsAgo, unitTwoMonthsAgo) =>
        assert(minTwoMonthsAgo <= twoMonthsAgo)
        assert(maxTwoMonthsAgo >= twoMonthsAgo)
        assert(unitTwoMonthsAgo == TimeUnit.Month)
    }

    TimeQuestionGenerator.generateRange(threeWeeksAgo) match {
      case (minThreeWeeksAgo, maxThreeWeeksAgo, unitThreeWeeksAgo) =>
        assert(minThreeWeeksAgo <= threeWeeksAgo)
        assert(maxThreeWeeksAgo >= threeWeeksAgo)
        assert(unitThreeWeeksAgo == TimeUnit.Week)
    }

    TimeQuestionGenerator.generateRange(fourDaysAgo) match {
      case (minFourDaysAgo, maxFourDaysAgo, unitFourDaysAgo) =>
        assert(minFourDaysAgo <= fourDaysAgo)
        assert(maxFourDaysAgo >= fourDaysAgo)
        assert(unitFourDaysAgo == TimeUnit.Day)
    }
  }
}