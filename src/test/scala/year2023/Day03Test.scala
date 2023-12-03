package year2023

import scala.util.Using
import scala.io.Source

class Day03Test extends org.scalatest.funsuite.AnyFunSuite {
  def parseInput(input: String): Seq[Seq[String]] =
    input
      .split("\n")
      .map(_.split("").toSeq)
      .toSeq

  val digitRegex = "\\d".r

  case class Acc(numbers: Seq[Number], currentNumber: Option[Number])
  case class Number(
      points: Seq[(Int, Int)],
      number: String,
      adjacentToSymbol: Boolean
  )

  def sol1(input: String): Int = {
    val points = parseInput(input)

    val zipped: Seq[(Int, Seq[(Int, String)])] = points.zipWithIndex.map {
      case (line, y) => y -> line.zipWithIndex.map(_.swap)
    }

    def isAdjacentToSymbol(
        x: Int,
        y: Int,
        points: Seq[Seq[String]]
    ): Boolean = {
      val adjacentPoints =
        for (
          i <- -1 to 1; j <- -1 to 1;
          if ((i, j) != (0, 0))
        ) yield (x + i, y + j)

      adjacentPoints.toSeq.find { case (x, y) =>
        points
          .lift(y)
          .flatMap(_.lift(x))
          .exists(char => !digitRegex.matches(char) && char != ".")
      }.isDefined

    }

    val parsedAcc = zipped
      .map {
        case (y, line) => {
          line.foldLeft(Acc(Seq.empty, None)) {
            case (acc, (x, char)) => {
              if (digitRegex.matches(char)) {
                val number = Number(
                  Seq(x -> y),
                  char,
                  adjacentToSymbol = isAdjacentToSymbol(x, y, points)
                )
                acc.copy(
                  currentNumber = Some(
                    acc.currentNumber
                      .map(currentNumber =>
                        currentNumber.copy(
                          points = currentNumber.points :+ (x -> y),
                          number = currentNumber.number + char,
                          adjacentToSymbol =
                            currentNumber.adjacentToSymbol || isAdjacentToSymbol(
                              x,
                              y,
                              points
                            )
                        )
                      )
                      .getOrElse(
                        Number(
                          points = Seq(x -> y),
                          number = char,
                          isAdjacentToSymbol(x, y, points)
                        )
                      )
                  )
                )
              } else {
                acc.copy(
                  numbers = acc.currentNumber
                    .map(currentNumber => acc.numbers :+ currentNumber)
                    .getOrElse(acc.numbers),
                  currentNumber = None
                )
              }
            }
          }

        }
      }
      .flatMap(x =>
        x.currentNumber
          .map(currentNumber => x.numbers :+ currentNumber)
          .getOrElse(x.numbers)
      )
      .filter(_.adjacentToSymbol)
      .map(_.number.toInt)
      .sum
    parsedAcc

  }
  def sol2(input: String): Int = {
    val points = parseInput(input)

    val zipped: Seq[(Int, Seq[(Int, String)])] = points.zipWithIndex.map {
      case (line, y) => y -> line.zipWithIndex.map(_.swap)
    }

    def isAdjacentToSymbol(
        x: Int,
        y: Int,
        points: Seq[Seq[String]]
    ): Boolean = {
      val adjacentPoints =
        for (
          i <- -1 to 1; j <- -1 to 1;
          if ((i, j) != (0, 0))
        ) yield (x + i, y + j)

      adjacentPoints.toSeq.find { case (x, y) =>
        points
          .lift(y)
          .flatMap(_.lift(x))
          .exists(char => !digitRegex.matches(char) && char != ".")
      }.isDefined

    }

    val parsedAcc = zipped
      .map {
        case (y, line) => {
          line.foldLeft(Acc(Seq.empty, None)) {
            case (acc, (x, char)) => {
              if (digitRegex.matches(char)) {
                val number = Number(
                  Seq(x -> y),
                  char,
                  adjacentToSymbol = isAdjacentToSymbol(x, y, points)
                )
                acc.copy(
                  currentNumber = Some(
                    acc.currentNumber
                      .map(currentNumber =>
                        currentNumber.copy(
                          points = currentNumber.points :+ (x -> y),
                          number = currentNumber.number + char,
                          adjacentToSymbol =
                            currentNumber.adjacentToSymbol || isAdjacentToSymbol(
                              x,
                              y,
                              points
                            )
                        )
                      )
                      .getOrElse(
                        Number(
                          points = Seq(x -> y),
                          number = char,
                          isAdjacentToSymbol(x, y, points)
                        )
                      )
                  )
                )
              } else {
                acc.copy(
                  numbers = acc.currentNumber
                    .map(currentNumber => acc.numbers :+ currentNumber)
                    .getOrElse(acc.numbers),
                  currentNumber = None
                )
              }
            }
          }

        }
      }
      .flatMap(x =>
        x.currentNumber
          .map(currentNumber => x.numbers :+ currentNumber)
          .getOrElse(x.numbers)
      )
      .filter(_.adjacentToSymbol)

    val adjacents = parsedAcc.map(number => {
      number -> adjacentToGearAtPoint(number, points)
    })

    val grouped = adjacents.filter(_._2.length === 1).groupBy(_._2.head)

    grouped.values
      .filter(_.length == 2)
      .map(_.map(_._1.number.toInt).product)
      .sum

  }

  def adjacentToGearAtPoint(
      number: Number,
      points: Seq[Seq[String]]
  ): Seq[(Int, Int)] = {
    number.points.flatMap { case (x, y) =>
      val adjacentPoints =
        for (
          i <- -1 to 1; j <- -1 to 1;
          if ((i, j) != (0, 0))
        ) yield (x + i, y + j)

      adjacentPoints.toSeq.filter { case (x, y) =>
        points
          .lift(y)
          .flatMap(_.lift(x))
          .exists(char => char == "*")
      }
    }.distinct
  }

  val testInput = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
""".trim()

  val dayName =
    this.getClass().getName().split("\\.").last.replace("Test", "")
  val inputFile = s"src/test/scala/year2023/${dayName}-input.txt"
  val skipTest = dayName == "DayXX"

  test("test input") {
    if (!skipTest) assert(sol1(testInput) == 4361)
  }

  test("test input sol2") {
    if (!skipTest) assert(sol2(testInput) == 467835)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(sol1(input) === 527369)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(sol2(input) === 73074886)
    }
  }

}
