package year2022

object Day06 extends App {

  def sol(buffSize: Int)(input: String): Int = {
    val allChars = input.split("")
    input
      .split("")
      .indices
      .find { case (currIndex) =>
        allChars.slice(currIndex - buffSize, currIndex).toSet.size == buffSize
      }
      .get
  }

  def sol1(input: String): Int = sol(4)(input)

  def sol2(input: String): Int = sol(14)(input)

}
