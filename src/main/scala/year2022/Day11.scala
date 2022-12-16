package year2022

import java.text.ParseException

object Day11 extends App {
  /*
val example = """Monkey 0:
                |  Starting items: 79, 98
                |  Operation: new = old * 19
                |  Test: divisible by 23
                |    If true: throw to monkey 2
                |    If false: throw to monkey 3""".stripMargin
   */
  case class Monkey(
      items: Seq[Long],
      op: Long => Long,
      div: Int,
      onTrue: Int,
      onFalse: Int,
      inspections: Int
  )

  def parseMonkey(input: String): Monkey = {
    val lines = input.split("\n")
    val itemsLine = lines(1)
    val opLine = lines(2)
    val testLine = lines(3)
    val consLine = lines(4)
    val elseLine = lines(5)

    val startingItems = ("""(\d+)""").r
      .findAllMatchIn(itemsLine)
      .map(_.group(1))
      .toSeq
      .map(_.toLong)

    def opParsed(a: Long): Long = {
      val Seq(lh, operator, rh) =
        """\s*Operation: new = ([\w\d]+) ([+*]) ([\w\d]+)""".r
          .findAllMatchIn(opLine)
          .flatMap(_.subgroups)
          .toSeq

      def resolveOperand(operand: String) = operand match {
        case "old" => a
        case x     => x.toInt
      }

      operator match {
        case "*" => resolveOperand(lh) * resolveOperand(rh)
        case "+" => resolveOperand(lh) + resolveOperand(rh)
      }
    }

    val Seq(div) =
      """  Test: divisible by ([\d]+)""".r
        .findAllMatchIn(testLine)
        .flatMap(_.subgroups)
        .toSeq
        .map(_.toInt)

    val consParsed =
      """    If true: throw to monkey (\d+)""".r
        .findFirstMatchIn(consLine)
        .get
        .group(1)
        .toInt
    val elseParsed =
      """    If false: throw to monkey (\d+)""".r
        .findFirstMatchIn(elseLine)
        .get
        .group(1)
        .toInt

    Monkey(startingItems, opParsed, div, consParsed, elseParsed, 0)
  }

  private def runRound(
      monkeys: Seq[Monkey],
      divide: (Long) => Long
  ): Seq[Monkey] = {
    monkeys.zipWithIndex.foldLeft(monkeys) { case (acc, (_, currIndex)) =>
      val currMonkey = acc(currIndex)
      val newMonkey = currMonkey.copy(
        inspections = currMonkey.inspections + currMonkey.items.size,
        items = Seq()
      )
      // (index, item)
      val newItemLocations = currMonkey.items.map(item => {
        val newItem = divide(currMonkey.op(item))
        val newIndex =
          if (newItem % currMonkey.div == 0) currMonkey.onTrue
          else currMonkey.onFalse
        (newIndex, newItem)
      })
      val updatedItems = newItemLocations.foldLeft(acc) {
        case (monkeys, (index, item)) => {
          val monkeyToUpdate = monkeys(index)
          monkeys.updated(
            index,
            monkeyToUpdate.copy(items = monkeyToUpdate.items.appended(item))
          )
        }
      }
      updatedItems.updated(currIndex, newMonkey)
    }
  }

  def sol1(input: String): Int = {

    val parsed = input.split("\n\n").toSeq
    val monkeys = parsed.map(parseMonkey)

    val rounds = (0 until 20).foldLeft(monkeys) {
      case (acc, _) => {
        val newRound = runRound(acc, (a) => a / 3)
        newRound
      }
    }

    rounds.map(_.inspections).sorted.reverse.take(2).product
  }

  def sol2(input: String, maxRounds: Int): Long = {
    val parsed = input.split("\n\n").toSeq
    val monkeys = parsed.map(parseMonkey)

    def getLcm(list: Seq[Int]): Int = list.foldLeft(1: Int) { (a, b) =>
      b * a /
        LazyList
          .iterate((a, b)) { case (x, y) => (y, x % y) }
          .dropWhile(_._2 != 0)
          .head
          ._1
          .abs
    }
    val dividers = monkeys.map(_.div)
    val lcm = getLcm(dividers)

    val rounds = (0 until maxRounds).foldLeft(monkeys) {
      case (acc, _) => {
        val newRound = runRound(
          acc,
          (a) => a % lcm
        )
        newRound
      }
    }

    val Seq(fst, snd) = rounds.map(_.inspections).sorted.reverse.take(2)
    fst.toLong * snd.toLong
  }

}
