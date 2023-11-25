package year2022

import scala.util.Using
import scala.io.Source

class Day13Test extends org.scalatest.funsuite.AnyFunSuite {
  val inputFile = "src/test/scala/year2022/Day13-input.txt"
  val testInput = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""
  val Day = Day13
  val skipTest = this.getClass().getName() == "year2022.DayXXTest"

  test("parseTree") {
    if (!skipTest) {
      assert(
        Day.parseTree("[1,2,3]") == Day.Branch(
          List(Day.Leaf(1), Day.Leaf(2), Day.Leaf(3))
        )
      )
      assert(
        Day.parseTree("[1,[2,3],4]") == Day.Branch(
          List(
            Day.Leaf(1),
            Day.Branch(List(Day.Leaf(2), Day.Leaf(3))),
            Day.Leaf(4)
          )
        )
      )
      assert(
        Day.parseTree("[1,[2,[3,4],5],6]") == Day.Branch(
          List(
            Day.Leaf(1),
            Day.Branch(
              List(
                Day.Leaf(2),
                Day.Branch(List(Day.Leaf(3), Day.Leaf(4))),
                Day.Leaf(5)
              )
            ),
            Day.Leaf(6)
          )
        )
      )
      assert(
        Day.parseTree("[1,[2,[3,[4,[5,6,7]]]],8,9]") == Day.Branch(
          List(
            Day.Leaf(1),
            Day.Branch(
              List(
                Day.Leaf(2),
                Day.Branch(
                  List(
                    Day.Leaf(3),
                    Day.Branch(
                      List(
                        Day.Leaf(4),
                        Day.Branch(
                          List(
                            Day.Leaf(5),
                            Day.Leaf(6),
                            Day.Leaf(7)
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            Day.Leaf(8),
            Day.Leaf(9)
          )
        )
      )
    }

  }

  test("right order") {
    import Day13._
    assert(Day13.rightOrder(Leaf(1), Leaf(2)) === Some(true))
    assert(Day13.rightOrder(Leaf(2), Leaf(1)) === Some(false))
    assert(
      Day13.rightOrder(
        parseTree("[1,1,3,1,1]"),
        parseTree("[1,1,5,1,1]")
      ) === Some(true)
    )
    assert(
      Day13.rightOrder(
        parseTree("[[1],[2,3,4]]"),
        parseTree("[[1],4]")
      ) === Some(true)
    )
    assert(
      Day13.rightOrder(
        parseTree("[9]"),
        parseTree("[[8,7,6]]")
      ) === Some(false)
    )
    assert(
      Day13.rightOrder(
        parseTree("[[4,4],4,4]"),
        parseTree("[[4,4],4,4,4]")
      ) === Some(true)
    )
    assert(
      Day13.rightOrder(
        parseTree("[7,7,7,7]"),
        parseTree("[7,7,7]")
      ) === Some(false)
    )
    assert(
      Day13.rightOrder(
        parseTree("[]"),
        parseTree("[3]")
      ) === Some(true)
    )
    assert(
      Day13.rightOrder(
        parseTree("[[[]]]"),
        parseTree("[[]]")
      ) === Some(false)
    )
    assert(
      Day13.rightOrder(
        parseTree("[1,[2,[3,[4,[5,6,7]]]],8,9]"),
        parseTree("[1,[2,[3,[4,[5,6,0]]]],8,9]")
      ) === Some(false)
    )

  }

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 13)
  }

  test("test input sol2") {
    if (!skipTest)
      assert(Day.sol2(testInput) == 140)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol1(input) === 6420)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input) === 22000)
    }
  }

}
