package year2022

import scala.util.Using
import scala.io.Source
class Day1Test extends org.scalatest.funsuite.AnyFunSuite {
  val testInput = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""
  test("test input") {
    assert(Day01.sol(testInput) == 24000)
  }

  test("test input sol2") {
    assert(Day01.sol2(testInput) == 45000)
  }

  test("real input") {
    val res =
      Using(Source.fromFile("src/test/scala/year2022/Day01-input.txt")) {
        source =>
          source.mkString
      }
    val input = res.get
    assert(Day01.sol(input) === 67633)
  }

  test("real input part2") {
    val res =
      Using(Source.fromFile("src/test/scala/year2022/Day01-input.txt")) {
        source =>
          source.mkString
      }
    val input = res.get
    assert(Day01.sol2(input) === 199628)
  }

}
