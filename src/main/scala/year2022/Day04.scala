package year2022

import java.text.ParseException
import scala.collection.immutable.HashSet

object Day04 extends App {
  // "1-4" => Set(1,2,3,4)
  def assignmentToSections(input: String): Set[Int] =
    input.split("-").map(_.toInt) match {
      case Array(start, end) => (start to end).toSet
    }

  def sol1(input: String): Int = {
    val pairs = input.split("\n")
    pairs
      .map(pair => {
        pair.split(",").map(assignmentToSections)
      })
      .map(_ match {
        case Array(leftSet, rightSet) => {
          val intersect = leftSet.union(rightSet)
          val shouldCount = leftSet == intersect || rightSet == intersect
          if (shouldCount) {
            1
          } else {
            0
          }
        }
      })
      .sum
  }
  def sol2(input: String): Int = {
    val pairs = input.split("\n")
    pairs
      .map(pair => {
        pair.split(",").map(assignmentToSections)
      })
      .map(_ match {
        case Array(leftSet, rightSet) => {
          val intersect = leftSet.union(rightSet)
          val leftCount = leftSet.size
          val rightCount = rightSet.size
          val anyOverlap = intersect.size < leftCount + rightCount
          if (anyOverlap) {
            1
          } else {
            0
          }
        }
      })
      .sum
  }

}
