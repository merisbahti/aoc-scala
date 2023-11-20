package year2022

import scala.io.Source
import scala.util.Using

class Day12Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day12-input.txt"
  val testInput =
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""".stripMargin
  val Day = Day12
  val skipTest = false

  test("possibleSteps") {
    val map = Day.parseMap(testInput)
    assert(Day.getPossibleSteps(map, (0, 0), Seq()) == Seq((0, 1), (1, 0)))
    assert(Day.getPossibleSteps(map, (1, 1), Seq()) == Seq((1, 2), (2, 1)))
    assert(
      Day.getPossibleSteps(map, (2, 2), Seq()) == List((1, 2), (2, 1), (2, 3))
    )
    assert(Day.getPossibleSteps(map, (4, 2), Seq()) == Seq((5, 2)))
    assert(Day.getPossibleSteps(map, (4, 2), Seq()) == Seq((5, 2)))
    assert(
      Day.getPossibleSteps(map, (2, 3), Seq()) == Seq((1, 3), (2, 2), (2, 4))
    )
    assert(
      Day.getPossibleSteps(map, (2, 3), Seq((2, 2))) == Seq((1, 3), (2, 4))
    )
  }

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 31)
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
