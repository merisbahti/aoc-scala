package year2022

import scala.util.Using
import scala.io.Source
class Day02Test extends org.scalatest.funsuite.AnyFunSuite {
  val testInput = """A Y
B X
C Z
"""
  test("test input") {
    assert(Day02.sol(testInput) == 15)
  }

  test("test input sol2") {
    assert(Day02.sol2(testInput) == 12)
  }

  test("real input") {
    val res =
      Using(Source.fromFile("src/test/scala/year2022/Day02-input.txt")) {
        source =>
          source.mkString
      }
    val input = res.get
    assert(Day02.sol(input) === 13005)
  }

  test("real input part2") {
    val res =
      Using(Source.fromFile("src/test/scala/year2022/Day02-input.txt")) {
        source =>
          source.mkString
      }
    val input = res.get
    assert(Day02.sol2(input) === 11373)
  }

}
