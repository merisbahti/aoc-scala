package year2022

import java.text.ParseException
import scala.util.parsing.combinator._

object Day13 extends App {

  sealed trait Tree
  case class Leaf(value: Int) extends Tree
  case class Branch(children: List[Tree]) extends Tree
  // Input on the form of [1,[2,[3,[4,[5,6,7]]]],8,9]

  class TreeParser extends RegexParsers {
    def tree = branch | number
    def number: Parser[Leaf] = """[0-9]+""".r ^^ { case (x) => Leaf(x.toInt) }
    def branch: Parser[Branch] = "[" ~> repsep(tree, ",") <~ "]" ^^ {
      case (x) =>
        Branch(x)
    }
  }

  def parseTree(input: String): Tree = {
    val treeParser = new TreeParser()
    treeParser.parse(treeParser.tree, input) match {
      case treeParser.Success(result, _) => result
      case treeParser.NoSuccess(msg, _)  => throw new ParseException(msg, 0)
      case _ => throw new ParseException("Unknown error", 0)
    }
  }

  def rightOrder(left: Tree, right: Tree): Option[Boolean] =
    (left, right) match {
      case (Leaf(left), Leaf(right)) => {
        if (left == right) None
        else if (left < right) Some(true)
        else Some(false)
      }
      case (Branch(leftChildren), Branch(rightChildren)) => {
        val maxLength = Math.max(leftChildren.length, rightChildren.length)

        val result =
          Seq.fill(maxLength)().zipWithIndex.foldLeft(Option.empty[Boolean]) {
            case (acc, (_, index)) =>
              if (acc.isDefined) return acc
              val leftChild = leftChildren.lift(index)
              val rightChild = rightChildren.lift(index)

              (leftChild, rightChild) match {
                case (None, Some(_))           => Some(true)
                case (Some(_), None)           => Some(false)
                case (Some(left), Some(right)) => rightOrder(left, right)
                case (None, None) =>
                  throw new RuntimeException("This should not happen")
              }
          }

        result
      }
      case (branch, Leaf(rightNumber)) => {
        rightOrder(branch, Branch(List(Leaf(rightNumber))))
      }
      case (Leaf(leftNumber), branch) => {
        rightOrder(Branch(List(Leaf(leftNumber))), branch)
      }
    }

  def sol1(input: String): Int = {
    val pairInputs = input.split("\n\n").toSeq
    val pairs = pairInputs.map(_.split("\n")).map { case Array(a, b) =>
      (parseTree(a), parseTree(b))
    }

    return pairs.zipWithIndex
      .flatMap((pair) => {
        val ((leftTree, rightTree), index) = pair
        println(leftTree)
        println(rightTree)
        println(rightOrder(leftTree, rightTree))
        if (rightOrder(leftTree, rightTree) == Some(true)) {
          Some(index + 1)
        } else {
          None
        }
      })
      .sum

  }

  def sol2(input: String): Int = {
    val originalTrees =
      input.split("\n\n").toSeq.flatMap(_.split("\n")).map(parseTree)
    val extraTrees = Seq(parseTree("[[2]]"), parseTree("[[6]]"))
    val trees = originalTrees ++ extraTrees

    val sorted = trees
      .sortWith((left, right) => {
        rightOrder(left, right) match {
          case Some(true)  => true
          case Some(false) => false
          case None        => true
        }
      })
      .zipWithIndex
      .map(x => (x._1, x._2 + 1))

    sorted.foreach(println)

    val foundItems = sorted.filter(x => extraTrees.contains(x._1))

    if (foundItems.length != 2) {
      throw new RuntimeException(
        s"Could not find exactly 2 trees, found these: $foundItems"
      )
    }

    return foundItems.map(_._2).product
  }

}
