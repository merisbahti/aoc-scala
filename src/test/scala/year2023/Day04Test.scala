package year2023

import scala.util.Using
import scala.io.Source

class Day04Test extends org.scalatest.funsuite.AnyFunSuite {
  def sol1(input: String): Int = {
    val firstSplit = input.split("\n")
    val lines = firstSplit.map { line =>
      val splitLine = line.split("\\s*\\|\\s*")
      if (splitLine.length != 2) {
        throw new Exception("Invalid input")
      }
      val winningStrings = splitLine.apply(1).trim().split("\\s+")

      if (
        winningStrings
          .map(_.toIntOption)
          .flatten
          .length != winningStrings.length
      ) {
        throw new Exception("Invalid input")
      }

      val winningNumbers = winningStrings.map(_.toInt)
      val myStrings =
        splitLine
          .apply(0)
          .split("\\s*:\\s*")
          .apply(1)
          .trim()
          .split("\\s+")

      if (
        myStrings
          .map(_.toIntOption)
          .flatten
          .length != myStrings.length
      ) {
        throw new Exception("Invalid input")
      }
      val myNumbers =
        myStrings.map(_.toInt)

      val wins = myNumbers.filter(winningNumbers.contains(_)).length

      if (wins == 0) {
        0
      } else {
        1 * Math.pow(2, wins - 1).toInt
      }
    }

    lines.sum
  }
  def sol2(input: String): Int = ???

  val testInput = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".trim()

  val dayName =
    this.getClass().getName().split("\\.").last.replace("Test", "")
  val inputFile = s"src/test/scala/year2023/${dayName}-input.txt"
  val skipTest = dayName == "DayXX"

  test("test input") {
    if (!skipTest) assert(sol1(testInput) == 13)
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
      assert(sol1(input) === 20117)
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
