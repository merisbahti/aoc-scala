package year2022

import scala.io.Source
import scala.util.Using

class Day05Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day05-input.txt"
  val testInput = """    [D]
                    |[N] [C]
                    |[Z] [M] [P]
                    | 1   2   3
                    |
                    |move 1 from 2 to 1
                    |move 3 from 1 to 3
                    |move 2 from 2 to 1
                    |move 1 from 1 to 2""".stripMargin
  val Day = Day05
  val skipTest = false

  test("parsing") {
    assert(
      Day.parseState(testInput.split("\n\n").head) == Seq(
        Seq("N", "Z"),
        Seq("D", "C", "M"),
        Seq("P")
      )
    )
  }

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == "CMZ")
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
      assert(Day.sol1(input) === 13005)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input) === "ABCD")
    }
  }

}
