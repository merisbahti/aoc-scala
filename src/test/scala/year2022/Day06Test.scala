package year2022

import scala.io.Source
import scala.util.Using

class Day06Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day06-input.txt"
  val testInputsp1 = Seq(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb" -> 7,
    "bvwbjplbgvbhsrlpgdmjqwftvncz" -> 5,
    "nppdvjthqldpwncqszvftbrmjlhg" -> 6,
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" -> 10,
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" -> 11
  )
  val testInputsp2 = Seq(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb" -> 19,
    "bvwbjplbgvbhsrlpgdmjqwftvncz" -> 23,
    "nppdvjthqldpwncqszvftbrmjlhg" -> 23,
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" -> 29,
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" -> 26
  )
  val Day = Day06
  val skipTest = false

  test("test input") {
    testInputsp1.foreach { case (input, expected) =>
      assert(Day.sol1(input) == expected)
    }
  }

  test("test input sol2") {
    testInputsp2.foreach { case (input, expected) =>
      assert(Day.sol2(input) == expected)
    }
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol1(input) === 1100)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input) === 2421)
    }
  }

}
