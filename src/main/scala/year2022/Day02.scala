package year2022

import java.text.ParseException

sealed trait Shape
final case object Rock extends Shape
final case object Paper extends Shape
final case object Scissor extends Shape

case class Match(op: Shape, you: Shape)

sealed trait Outcome
final case object Loss extends Outcome
final case object Draw extends Outcome
final case object Win extends Outcome

case class Row(op: Shape, outcome: Outcome)

object Day02 extends App {

  def parse(row: String): Match = {
    val Pattern = "([ABC]) ([XYZ])".r
    row match {
      case Pattern(opStr, youStr) => {
        val you = youStr match {
          case "X" => Rock
          case "Y" => Paper
          case "Z" => Scissor
        }
        val op = opStr match {
          case "A" => Rock
          case "B" => Paper
          case "C" => Scissor
        }
        Match(op, you)
      }
    }
  }

  def calcScore(m: Match): Int = {
    val shapeScore = m.you match {
      case Rock    => 1
      case Paper   => 2
      case Scissor => 3
    }
    val outcomeScore = m match {
      // wins
      case Match(Paper, Scissor) => 6
      case Match(Scissor, Rock)  => 6
      case Match(Rock, Paper)    => 6

      // draws
      case Match(Rock, Rock)       => 3
      case Match(Paper, Paper)     => 3
      case Match(Scissor, Scissor) => 3

      // losses
      case Match(Scissor, Paper) => 0
      case Match(Rock, Scissor)  => 0
      case Match(Paper, Rock)    => 0
    }
    return shapeScore + outcomeScore
  }

  def sol(input: String): Int = {
    val parsed = input.split("\n").map(parse)
    val calced = parsed.map(calcScore)
    return calced.sum
  }

  def parseSol2(row: String): Row = {
    val Pattern = "([ABC]) ([XYZ])".r
    row match {
      case Pattern(opStr, youStr) => {
        val op = opStr match {
          case "A" => Rock
          case "B" => Paper
          case "C" => Scissor
        }
        val outcome = youStr match {
          case "X" => Loss
          case "Y" => Draw
          case "Z" => Win
        }
        Row(op, outcome)
      }
    }
  }

  def calcScore2(row: Row): Int = {
    val shapeScore = row match {
      // Wins
      case Row(Scissor, Win) => 1
      case Row(Rock, Win)    => 2
      case Row(Paper, Win)   => 3

      // Losses
      case Row(Scissor, Loss) => 2
      case Row(Rock, Loss)    => 3
      case Row(Paper, Loss)   => 1

      // Draws
      case Row(Rock, Draw)    => 1
      case Row(Paper, Draw)   => 2
      case Row(Scissor, Draw) => 3
    }
    val outcomeScore = row.outcome match {
      case Draw => 3
      case Loss => 0
      case Win  => 6
    }
    return shapeScore + outcomeScore
  }

  def sol2(input: String): Integer = {
    val parsed = input.split("\n").map(parseSol2)
    val calced = parsed.map(calcScore2)
    return calced.sum
  }

}
