package year2022

object Day01 extends App {

  def sol(input: String): Integer =
    input.split("\n\n").map(_.split("\n").map(Integer.parseInt(_)).sum).max

}
