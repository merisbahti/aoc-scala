package year2023

import scala.util.Using
import scala.io.Source

class DayXXTest extends org.scalatest.funsuite.AnyFunSuite {
  def sol1(input: String): Int = ???
  def sol2(input: String): Int = ???

  val testInput = """"""

  val dayName =
    this.getClass().getName().split("\\.").last.replace("Test", "")
  val inputFile = s"src/test/scala/year2023/${dayName}-input.txt"
  val skipTest = dayName == "DayXX"

  test("test input") {
    if (!skipTest) assert(sol1(testInput) == -1)
  }

  test("test input sol2") {
    if (!skipTest) assert(sol2(testInput) == -1)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(sol1(input) === -1)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(sol2(input) === -1)
    }
  }

}
