package year2022

import scala.io.Source
import scala.util.Using

class Day07Test extends org.scalatest.funsuite.AnyFunSuite {
  /* CHANGE THESE */
  val inputFile = "src/test/scala/year2022/Day07-input.txt"
  val testInput: String = """$ cd /
                            |$ ls
                            |dir a
                            |14848514 b.txt
                            |8504156 c.dat
                            |dir d
                            |$ cd a
                            |$ ls
                            |dir e
                            |29116 f
                            |2557 g
                            |62596 h.lst
                            |$ cd e
                            |$ ls
                            |584 i
                            |$ cd ..
                            |$ cd ..
                            |$ cd d
                            |$ ls
                            |4060174 j
                            |8033020 d.log
                            |5626152 d.ext
                            |7214296 k""".stripMargin
  val Day: Day07.type = Day07
  val skipTest = false

  test("test input") {
    if (!skipTest)
      assert(Day.sol1(testInput) == 15)
  }

  test("test input sol2") {
    if (!skipTest)
      assert(Day.sol2(testInput) == 24933642)
  }

  test("real input") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol1(input) === 1648397)
    }
  }

  test("real input part2") {
    if (!skipTest) {
      val res =
        Using(Source.fromFile(inputFile)) { source =>
          source.mkString
        }
      val input = res.get
      assert(Day.sol2(input) === 1815525)
    }
  }

}
