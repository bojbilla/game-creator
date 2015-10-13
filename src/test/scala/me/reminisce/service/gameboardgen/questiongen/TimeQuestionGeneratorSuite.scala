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

    val (minOneYearAgo, maxOneYearAgo, unitOneYearAgo) = TimeQuestionGenerator.generateRange(oneYearAgo)
    assert(minOneYearAgo <= oneYearAgo)
    assert(maxOneYearAgo >= oneYearAgo)
    assert(unitOneYearAgo == TimeUnit.Year)

    val (minTwoYearsAgo, maxTwoYearsAgo, unitTwoYearsAgo) = TimeQuestionGenerator.generateRange(twoYearsAgo)
    assert(minTwoYearsAgo <= twoYearsAgo)
    assert(maxTwoYearsAgo >= twoYearsAgo)
    assert(unitTwoYearsAgo == TimeUnit.Year)

    val (minTwoMonthsAgo, maxTwoMonthsAgo, unitTwoMonthsAgo) = TimeQuestionGenerator.generateRange(twoMonthsAgo)
    assert(minTwoMonthsAgo <= twoMonthsAgo)
    assert(maxTwoMonthsAgo >= twoMonthsAgo)
    assert(unitTwoMonthsAgo == TimeUnit.Month)

    val (minThreeWeeksAgo, maxThreeWeeksAgo, unitThreeWeeksAgo) = TimeQuestionGenerator.generateRange(threeWeeksAgo)
    assert(minThreeWeeksAgo <= threeWeeksAgo)
    assert(maxThreeWeeksAgo >= threeWeeksAgo)
    assert(unitThreeWeeksAgo == TimeUnit.Week)

    val (minFourDaysAgo, maxFourDaysAgo, unitFourDaysAgo) = TimeQuestionGenerator.generateRange(fourDaysAgo)
    assert(minFourDaysAgo <= fourDaysAgo)
    assert(maxFourDaysAgo >= fourDaysAgo)
    assert(unitFourDaysAgo == TimeUnit.Day)
  }
}