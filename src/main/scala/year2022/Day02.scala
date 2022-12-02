package year2022

import java.text.ParseException

sealed trait Shape
final case object Rock extends Shape
final case object Paper extends Shape
final case object Scissor extends Shape

case class Match(op: Shape, you: Shape)

object Day02 extends App {

  def parse(input: String): Array[Match] = {
    val lines = input.split("\n");
    val Pattern = "([ABC]) ([XYZ])".r
    lines.map(row => {
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
    })
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
    val parsed = parse(input)
    val calced = parsed.map(calcScore)
    return calced.sum
  }

  def sol2(input: String): Integer = {
    ???
  }

}
