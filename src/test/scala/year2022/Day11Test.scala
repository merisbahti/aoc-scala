package year2022

import scala.util.Using
import scala.io.Source

class Day11Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day11-input.txt"
  val testInput = """Monkey 0:
                    |  Starting items: 79, 98
                    |  Operation: new = old * 19
                    |  Test: divisible by 23
                    |    If true: throw to monkey 2
                    |    If false: throw to monkey 3
                    |
                    |Monkey 1:
                    |  Starting items: 54, 65, 75, 74
                    |  Operation: new = old + 6
                    |  Test: divisible by 19
                    |    If true: throw to monkey 2
                    |    If false: throw to monkey 0
                    |
                    |Monkey 2:
                    |  Starting items: 79, 60, 97
                    |  Operation: new = old * old
                    |  Test: divisible by 13
                    |    If true: throw to monkey 1
                    |    If false: throw to monkey 3
                    |
                    |Monkey 3:
                    |  Starting items: 74
                    |  Operation: new = old + 3
                    |  Test: divisible by 17
                    |    If true: throw to monkey 0
                    |    If false: throw to monkey 1""".stripMargin
  val Day = Day11
  val skipTest = false

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 10605)
  }

  test("test input sol2") {
    if (!skipTest) {
      assert(Day.sol2(testInput, 10000) == 2713310158L)
    }
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol1(input) === 118674)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input, 10000) === 32333418600f)
    }
  }

}
