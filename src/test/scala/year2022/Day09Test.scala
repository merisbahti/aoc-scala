package year2022

import scala.io.Source
import scala.util.Using
import Day09.{moveTail, Point}

class Day09Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day09-input.txt"
  val testInput =
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin
  val Day = Day09
  val skipTest = false

  test("move tail") {
    assert(moveTail(Point(1, 1), Point(2, 3)) == Point(2, 2))
    assert(moveTail(Point(1, 1), Point(3, 1)) == Point(2, 1))
    assert(moveTail(Point(3, 0), Point(4, 1)) == Point(3, 0))
    assert(moveTail(Point(-1, -1), Point(-3, -1)) == Point(-2, -1))
    assert(moveTail(Point(-1, -1), Point(-2, -3)) == Point(-2, -2))
  }

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 13)
  }

  test("test input sol2") {
    if (!skipTest)
      assert(Day.sol2(testInput) == 1)
  }

  test("test input 2 sol2") {
    val input =
      """R 5
        |U 8
        |L 8
        |D 3
        |R 17
        |D 10
        |L 25
        |U 20""".stripMargin
    assert(Day.sol2(input) == 36)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol1(input) === 6391)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input) === 2593)
    }
  }

}
