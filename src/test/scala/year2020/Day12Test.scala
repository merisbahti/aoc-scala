package year2020

import scala.io.Source
import scala.util.Using

class Day12Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2020/Day12-input.txt"
  val testInput =
    """F10
      |N3
      |F7
      |R90
      |F11""".stripMargin
  val Day = Day12
  val skipTest = false

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 25)
  }

  test("test input sol2") {
    if (!skipTest)
      assert(Day.sol2(testInput) == 286)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol1(input) === 938)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input) === 54404)
    }
  }

}
