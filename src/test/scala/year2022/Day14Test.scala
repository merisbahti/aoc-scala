package year2022

import scala.util.Using
import scala.io.Source

class Day14Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day14-input.txt"
  val testInput = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""
  val Day = Day14
  val skipTest = this.getClass().getName() == "year2022.DayXXTest"

  test(s"parse first line: ${testInput.split("\n")(0)}") {
    val input = testInput.split("\n")(0)
    val expected = List((498, 4), (498, 5), (498, 6), (497, 6), (496, 6))
    assert(Day.parsePath(input) == expected)
  }

  test(s"parse second line: ${testInput.split("\n")(1)}") {
    val input = testInput.split("\n")(1)
    val expected = List(
      (503, 4),
      (502, 4),
      (502, 5),
      (502, 6),
      (502, 7),
      (502, 8),
      (502, 9),
      (501, 9),
      (500, 9),
      (499, 9),
      (498, 9),
      (497, 9),
      (496, 9),
      (495, 9),
      (494, 9)
    )
    assert(Day.parsePath(input) == expected)
  }

  test("test parse") {
    val input = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""

    val stuff = """
""".strip()
  }

  test("test parse printed") {
    val input = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""

    assert(Day14.printCaveMap(Day14.parse(input)) == """....#...##
....#...#.
..###...#.
........#.
........#.
#########.""")
  }

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 15)
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
      assert(Day.sol2(input) === 11373)
    }
  }

}
