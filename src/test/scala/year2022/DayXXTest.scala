package year2022

import scala.util.Using
import scala.io.Source

class DayXXTest extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day02-input.txt"
  val testInput = """"""
  val Day = Day02
  val skipTest = true

  test("test input") {
    if (!skipTest)
      assert(Day.sol(testInput) == 15)
  }

  test("test input sol2") {
    if (!skipTest)
      assert(Day.sol2(testInput) == 12)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol(input) === 13005)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input) === 11373)
    }
  }

}
