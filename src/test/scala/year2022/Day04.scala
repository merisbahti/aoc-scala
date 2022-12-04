package year2022

import scala.util.Using
import scala.io.Source

class Day04Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day04-input.txt"
  val testInput = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""
  val Day = Day04
  val skipTest = false

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 2)
  }

  test("test input sol2") {
    if (!skipTest)
      assert(Day.sol2(testInput) == 4)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol1(input) === 582)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input) === 893)
    }
  }

}
