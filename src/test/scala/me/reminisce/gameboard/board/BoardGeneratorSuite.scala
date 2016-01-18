package me.reminisce.gameboard.board

import org.scalatest.FunSuite


class BoardGeneratorSuite extends FunSuite {

  test("Uniform picking should not pick an element with a count of 0.") {
    val bagSizes = List(0, 14, 13)
    val bagTypes = List("None", "Some1", "Some2")
    val quantity = 10
    val pickedTypes = BoardGenerator.drawUniformlyFromBags[String](bagSizes, bagTypes, quantity)
    assert(pickedTypes.count(s => s == "Some1") == 5)
    assert(pickedTypes.count(s => s == "Some2") == 5)
    assert(!pickedTypes.contains("None"))
  }

}
