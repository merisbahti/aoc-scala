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
  case class Monkey(items: Seq[Int], op: Int => Int, test: Int => Boolean, onTrue: Int, onFalse: Int, inspections: Int)
  def parseMonkey(input: String): Monkey = {
    val lines = input.split("\n")
    val itemsLine = lines(1)
    val opLine = lines(2)
    val testLine = lines(3)
    val consLine = lines(4)
    val elseLine = lines(5)

    val startingItems = ("""(\d+)""").r.findAllMatchIn(itemsLine).map(_.group(1)).toSeq.map(_.toInt)
    def opParsed(a: Int): Int = {
      val Seq(lh, operator, rh) = """\s*Operation: new = ([\w\d]+) ([+*]) ([\w\d]+)""".r.findAllMatchIn(opLine).flatMap(_.subgroups).toSeq
      def resolveOperand(operand: String) = operand match {
        case "old" => a
        case x => x.toInt
      }
      operator match {
        case "*" => resolveOperand(lh) * resolveOperand(rh)
        case "+" => resolveOperand(lh) + resolveOperand(rh)
      }
    }
    def testParsed(a: Int): Boolean = {
      val Seq(div) = """  Test: divisible by ([\d]+)""".r.findAllMatchIn(testLine).flatMap(_.subgroups).toSeq.map(_.toInt)
      a % div == 0
    }

    val consParsed = """    If true: throw to monkey (\d+)""".r.findFirstMatchIn(consLine).get.group(1).toInt
    val elseParsed = """    If false: throw to monkey (\d+)""".r.findFirstMatchIn(elseLine).get.group(1).toInt


    Monkey(startingItems, opParsed, testParsed, consParsed, elseParsed, 0)
  }

  private def runRound(monkeys: Seq[Monkey]): Seq[Monkey] = {
    monkeys.zipWithIndex.foldLeft(monkeys) {case (acc, (_, currIndex)) =>
      val currMonkey = acc(currIndex)
      val newMonkey = currMonkey.copy(
        inspections = currMonkey.inspections + currMonkey.items.size,
        items = Seq()
      )
      // (index, item)
      val newItemLocations = currMonkey.items.map(item => {
        val newItem = currMonkey.op(item) / 3
        val newIndex = if (currMonkey.test(newItem) ) currMonkey.onTrue else currMonkey.onFalse
        (newIndex, newItem)
      })
      val updatedItems = newItemLocations.foldLeft(acc) {case (monkeys, (index, item)) => {
        val monkeyToUpdate = monkeys(index)
        monkeys.updated(index, monkeyToUpdate.copy(items = monkeyToUpdate.items.appended(item)))
      }}
      updatedItems.updated(currIndex, newMonkey)
    }
  }

  def sol1(input: String): Int = {


    val parsed = input.split("\n\n").toSeq
    val monkeys = parsed.map(parseMonkey)

    val rounds = (0 until 20).foldLeft(monkeys) {case (acc, _) => {
      val newRound = runRound(acc)
      newRound
    }}

    rounds.map(_.inspections).sorted.reverse.take(2).reduce(_*_)
  }
  def sol2(input: String): Int = ???

}
