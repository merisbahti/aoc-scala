package year2022

object Day10 extends App {

  def executeProgram(input: String) = {
    val instructions = input.split("\n")
    val regex = "(\\w+)( -?\\d+)?".r
    val parsed = instructions.map {
      case regex("noop", null) => ("noop", 0)
      case regex("addx", abc)  => ("addx", abc.trim.toInt)
    }

    parsed.foldLeft(Seq[Int](1)) {
      case (acc, ("noop", _)) => {
        acc.appended(acc.last)
      }
      case (acc, ("addx", x)) => {
        acc.appendedAll(Seq(acc.last, acc.last + x))
      }
    }
  }

  def sol1(input: String): Int = {
    val execution = executeProgram(input)

    val interestingValues = Seq(20, 60, 100, 140, 180, 220).zipWithIndex.map {
      case (v, i) =>
        v * execution.lift(v - 1).getOrElse(0)
    }
    interestingValues.reduce(_ + _)
  }

  def sol2(input: String): String = {
    val execution = executeProgram(input)
    val screen = {
      (0 to 5)
        .map { case (y) =>
          (0 to 39)
            .map {
              case (x) => {
                val index = y * 40 + x
                val value = execution(index)
                if (x <= (value + 1) && x >= (value - 1)) "#" else "."
              }
            }
            .mkString("")
        }
        .mkString("\n")
    }
    screen
  }

}
