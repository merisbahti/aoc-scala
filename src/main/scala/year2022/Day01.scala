package year2022

object Day01 extends App {

  def sol(input: String): Integer =
    input.split("\n\n").map(_.split("\n").map(Integer.parseInt(_)).sum).max

  def sol2(input: String): Integer = {

    val sums = input
      .split("\n\n")
      .map(_.split("\n").map(Integer.parseInt(_)).sum)

    return sums.sorted.reverse.take(3).sum

  }

}
