package year2023

import scala.util.Using
import scala.io.Source

class Day02Test extends org.scalatest.funsuite.AnyFunSuite {
  case class Game(
      maxBlue: Integer,
      maxRed: Integer,
      maxGreen: Integer
  )

  def parseLine(line: String): Game = {
    def getMaxColor(color: String) =
      s"""(\\d+) ${color}""".r
        .findAllMatchIn(line)
        .toSeq
        .map(_.group(1).toInt)
        .max
    val maxBlue = getMaxColor("blue")
    val maxRed = getMaxColor("red")
    val maxGreen = getMaxColor("green")
    return Game(maxBlue, maxRed, maxGreen)
  }

  def sol1(input: String): Int = {
    val games = input.split("\n").map(parseLine)
    val legitGames = games.zipWithIndex.filter {
      case (game, _) => {
        game.maxRed <= 12 && game.maxBlue <= 14 && game.maxGreen <= 13
      }
    }

    legitGames.map(x => x._2 + 1).sum
  }
  def sol2(input: String): Int = {
    val games = input.split("\n").map(parseLine).toSeq

    games.map(x => x.maxBlue * x.maxGreen * x.maxRed).sum
  }

  val testInput = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""

  val dayName =
    this.getClass().getName().split("\\.").last.replace("Test", "")
  val inputFile = s"src/test/scala/year2023/${dayName}-input.txt"
  val skipTest = dayName == "DayXX"

  test("test input") {
    if (!skipTest) assert(sol1(testInput) == 8)
  }

  test("test input sol2") {
    if (!skipTest) assert(sol2(testInput) == 2286)
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
      assert(sol2(input) === 56580)
    }
  }

}
