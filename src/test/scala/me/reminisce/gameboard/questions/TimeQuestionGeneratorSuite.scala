package me.reminisce.gameboard.questions

import com.github.nscala_time.time.Imports._
import me.reminisce.gameboard.questions.TimeQuestionGenerator._
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
      case (minOneYearAgo, defaultOneYearAgo, maxOneYearAgo, unitOneYearAgo, stepOneYearAgo) =>
        assert(minOneYearAgo <= oneYearAgo)
        assert(maxOneYearAgo >= oneYearAgo)
        assert(minOneYearAgo <= defaultOneYearAgo)
        assert(defaultOneYearAgo <= maxOneYearAgo)
        val step = unitToPeriod(unitOneYearAgo).multipliedBy(stepOneYearAgo)
        assert(minOneYearAgo + step.multipliedBy(QuestionGenerationConfig.timelineStepsNumber) == maxOneYearAgo)
    }

    TimeQuestionGenerator.generateRange(twoYearsAgo) match {
      case (minTwoYearsAgo, defaultTwoYearsAgo, maxTwoYearsAgo, unitTwoYearsAgo, stepTwoYearsAgo) =>
        assert(minTwoYearsAgo <= twoYearsAgo)
        assert(maxTwoYearsAgo >= twoYearsAgo)
        assert(minTwoYearsAgo <= defaultTwoYearsAgo)
        assert(defaultTwoYearsAgo <= maxTwoYearsAgo)
        val step = unitToPeriod(unitTwoYearsAgo).multipliedBy(stepTwoYearsAgo)
        assert(minTwoYearsAgo + step.multipliedBy(QuestionGenerationConfig.timelineStepsNumber) == maxTwoYearsAgo)
    }

    TimeQuestionGenerator.generateRange(twoMonthsAgo) match {
      case (minTwoMonthsAgo, defaultTwoMonthsAgo, maxTwoMonthsAgo, unitTwoMonthsAgo, stepTwoMonthsAgo) =>
        assert(minTwoMonthsAgo <= twoMonthsAgo)
        assert(maxTwoMonthsAgo >= twoMonthsAgo)
        assert(minTwoMonthsAgo <= defaultTwoMonthsAgo)
        assert(defaultTwoMonthsAgo <= maxTwoMonthsAgo)
        val step = unitToPeriod(unitTwoMonthsAgo).multipliedBy(stepTwoMonthsAgo)
        assert(minTwoMonthsAgo + step.multipliedBy(QuestionGenerationConfig.timelineStepsNumber) == maxTwoMonthsAgo)
    }

    TimeQuestionGenerator.generateRange(threeWeeksAgo) match {
      case (minThreeWeeksAgo, defaultThreeWeeksAgo, maxThreeWeeksAgo, unitThreeWeeksAgo, stepThreeWeeksAgo) =>
        assert(minThreeWeeksAgo <= threeWeeksAgo)
        assert(maxThreeWeeksAgo >= threeWeeksAgo)
        assert(minThreeWeeksAgo <= defaultThreeWeeksAgo)
        assert(defaultThreeWeeksAgo <= maxThreeWeeksAgo)
        val step = unitToPeriod(unitThreeWeeksAgo).multipliedBy(stepThreeWeeksAgo)
        assert(minThreeWeeksAgo + step.multipliedBy(QuestionGenerationConfig.timelineStepsNumber) == maxThreeWeeksAgo)
    }

    TimeQuestionGenerator.generateRange(fourDaysAgo) match {
      case (minFourDaysAgo, defaultFourDaysAgo, maxFourDaysAgo, unitFourDaysAgo, stepFourDaysAgo) =>
        assert(minFourDaysAgo <= fourDaysAgo)
        assert(maxFourDaysAgo >= fourDaysAgo)
        assert(minFourDaysAgo <= defaultFourDaysAgo)
        assert(defaultFourDaysAgo <= maxFourDaysAgo)
        val step = unitToPeriod(unitFourDaysAgo).multipliedBy(stepFourDaysAgo)
        assert(minFourDaysAgo + step.multipliedBy(QuestionGenerationConfig.timelineStepsNumber) == maxFourDaysAgo)
    }
  }
}