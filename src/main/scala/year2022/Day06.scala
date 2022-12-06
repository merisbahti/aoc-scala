package year2022

object Day06 extends App {

  def sol(buffSize: Int)(input: String): Int =
    input
      .split("")
      .sliding(buffSize)
      .zipWithIndex
      .find(_._1.toSet.size == buffSize)
      .map(_._2 + buffSize)
      .get

  def sol1(input: String): Int = sol(4)(input)

  def sol2(input: String): Int = sol(14)(input)

}
