package year2020

import scala.io.Source
import scala.util.Using

class Day13Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2020/Day13-input.txt"
  val testInput =
    """939
      |7,13,x,x,59,x,31,19""".stripMargin
  val Day = Day13
  val skipTest = true

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 295)
  }

  test("test input sol2") {
    if (!skipTest)
      assert(Day.sol2(testInput) == 1068788)
  }

  test("extra sol2 tests") {
    if (!skipTest)
      Seq(
        ("1789,37,47,1889", 1202161486),
        ("17,x,13,19", 3417),
        ("67,7,59,61", 754018),
        ("67,7,x,59,61", 1261476),
        ("67,x,7,59,61", 779210)
      ).foreach(x => assert(Day.sol2(s"\n${x._1}") == x._2))
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol1(input) === 4135)
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
