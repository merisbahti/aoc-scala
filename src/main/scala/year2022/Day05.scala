package year2022

object Day05 extends App {
  /*
        [D]
    [N] [C]
    [Z] [M] [P]
     1   2   3
   */
  /*
   Seq(
     Seq("N", "Z")
     Seq("D", "C", "M")
     Seq("P")
   )
   */
  def parseState(input: String): Seq[Seq[String]] = {
    val rawLines = input.split("\n")
    val rawIndexes = rawLines.reverse.head
    val rest = rawLines.reverse.tail.reverse.map(_.split("").toSeq).toSeq
    val indexes = rawIndexes.zipWithIndex
      .filter { case (c, _) =>
        !c.toString().isBlank()
      }
      .map(_._2)
    val stacks = indexes
      .map { case index =>
        rest
          .map(line => line.lift(index))
          .filter(x => !x.isEmpty && !x.exists(_.isBlank))
          .map(_.get)
      }
    stacks
  }

  case class Instruction(move: Int, fromIndex: Int, toIndex: Int)

  def parseInstructions(input: String): Seq[Instruction] = {
    val re = "move (\\d+) from (\\d+) to (\\d+)".r
    input.split("\n").map {
      _ match {
        case re(move, fromIndex, toIndex) =>
          Instruction(move.toInt, fromIndex.toInt - 1, toIndex.toInt - 1)
      }
    }
  }

  def runInstruction(state: Seq[Seq[String]], instruction: Instruction) = {
    val from = state.apply(instruction.fromIndex)
    val to = state.apply(instruction.toIndex)
    from.take(instruction.move)
    val newFrom = from.slice(instruction.move, from.length)
    val newTo = to.prependedAll(from.slice(0, instruction.move).reverse)
    val newState = state
      .updated(instruction.fromIndex, newFrom)
      .updated(instruction.toIndex, newTo)
    newState
  }

  def sol1(input: String): String = {
    val (state, instructions) = input.split("\n\n").toSeq match {
      case Seq(rawInitialState, rawInstructions) => {
        (parseState(rawInitialState), parseInstructions(rawInstructions))
      }
    }
    val end = instructions.foldLeft(state) { case (state, instruction) =>
      runInstruction(state, instruction)
    }
    end.map(_.head).reduce(_ + _)

  }
  def sol2(input: String): Int = ???

}
