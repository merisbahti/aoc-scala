package year2022

import scala.io.Source
import scala.util.Using

class Day08Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day08-input.txt"
  val testInput = """30373
                    |25512
                    |65332
                    |33549
                    |35390""".stripMargin
  val Day = Day08
  val skipTest = false

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 21)
  }

  test("test input sol2") {
    if (!skipTest)
      assert(Day.sol2(testInput) == 8)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol1(input) === 1560)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input) === 252000)
    }
  }

}
