package year2020

import scala.util.Using
import scala.io.Source
class Day11Test extends org.scalatest.funsuite.AnyFunSuite {
  val simpleExample = """L.L
LLL
.#.
"""
  test("adjacent") {
    val parsed = Day11.parse(simpleExample)
    assert(Day11.getAdjacentOccupied(1, 1, parsed) === 1)
    assert(Day11.getAdjacentOccupied(0, 0, parsed) === 0)
    assert(Day11.getAdjacentOccupied(2, 2, parsed) === 1)
    assert(Day11.getAdjacentOccupied(2, 0, parsed) === 0)
    assert(Day11.getAdjacentOccupied(0, 2, parsed) === 1)
    assert(Day11.getAdjacentOccupied(1, 2, parsed) === 0)
  }
  val testInput = """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
"""
  test("test input") {
    assert(Day11.sol(testInput) == 37)
  }

  test("real input") {
    val res =
      Using(Source.fromFile("src/test/scala/year2020/Day11-input.txt")) {
        source =>
          source.mkString
      }
    val input = res.get
    assert(Day11.sol(input) === 2329)
  }

}
