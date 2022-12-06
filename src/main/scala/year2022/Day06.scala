package year2022

object Day06 extends App {

  def sol(buffSize: Int)(input: String): Int = {
    val allChars = input.split("")
    val index = {
      allChars.zipWithIndex.foldLeft[Option[Int]](
        None
      ) {
        case (index, (currChar, currIndex)) => {
          val buffer = allChars.slice(currIndex - buffSize, currIndex)
          if (index.isEmpty && buffer.toSet.size == buffSize) {
            Some(currIndex)
          } else {
            index
          }
        }
      }
    }
    index.get
  }

  def sol1(input: String): Int = sol(4)(input)

  def sol2(input: String): Int = sol(14)(input)

}
