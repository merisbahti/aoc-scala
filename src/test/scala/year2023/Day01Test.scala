package year2023

import scala.util.Using
import scala.io.Source
import scala.util.Try

class Day01Test extends org.scalatest.funsuite.AnyFunSuite {
  def sol1(input: String): Int = input
    .split("\n")
    .map(line => {

      val digits =
        line.split("").filter(x => Try(Integer.parseInt(x)).isSuccess)

      val first = digits.apply(0)
      val last = digits.reverse.apply(0)

      Integer.parseInt(first + last)
    })
    .sum
  def sol2(input: String): Int = {
    val needles = Seq(
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine"
    )

    val regex = (needles :+ "\\d").mkString("|").r

    def parseMatched(matched: String): String = {
      val index = needles.indexOf(matched)
      if (index != -1) { (index + 1).toString }
      else matched
    }
    input
      .split("\n")
      .map(lines => {
        val matches = regex.findAllMatchIn(lines).toSeq
        val first = parseMatched(matches(0).toString())
        val last = parseMatched(matches.reverse(0).toString())
        Integer.parseInt(first + last)
      })
      .sum

  }

  val testInput = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

  val dayName =
    this.getClass().getName().split("\\.").last.replace("Test", "")
  val inputFile = s"src/test/scala/year2023/${dayName}-input.txt"
  val skipTest = dayName == "DayXX"

  test("test input") {
    if (!skipTest) assert(sol1(testInput) == 142)
  }

  val testinput2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""

  test("test input sol2") {
    if (!skipTest) assert(sol2(testinput2) == 281)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(sol1(input) == 54597)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(sol2(input) == -1)
    }
  }

}
