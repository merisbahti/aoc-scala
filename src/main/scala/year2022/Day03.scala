package year2022

import java.text.ParseException
import java.util.stream.Collectors
import scala.collection.JavaConverters._

object Day03 extends App {

  def stringToPrio(charString: String) = {
    val az = ('a' to 'z').map(_.toString())
    val azUpper = ('A' to 'Z').map(_.toString())
    val all = az.appendedAll(azUpper)
    val prio = all.indexOf(charString)
    if (prio < 0) {
      throw new Exception(
        s"Didn't expect prio to be 0, asked for char: ${charString}"
      )
    }
    prio + 1
  }

  def stringToSet(str: CharSequence) = str
    .toString()
    .split("")
    .toSet

  def sol1(input: String): Int = {
    val rucksacks = input.split("\n")
    rucksacks.map { rucksack =>
      {
        val firstCompartment = stringToSet(
          rucksack
            .subSequence(0, rucksack.length() / 2)
        )
        val secondCompartment = stringToSet(
          rucksack
            .subSequence(rucksack.length() / 2, rucksack.length())
        )

        val intersection = firstCompartment.intersect(secondCompartment)

        stringToPrio(intersection.head)
      }
    }.sum
  }

  def sol2(input: String): Int = {
    val rucksacks = input.split("\n")
    val rucksackGroups = rucksacks.zipWithIndex
      .groupBy { case (_, index) => (index / 3).floor }
      .map(_._2.map(_._1))
      .map(_.map(_.toSet))
    val intersections = rucksackGroups
      .map {
        case Array(a, b, c) => { a.intersect(b).intersect(c).toSeq }
      }
      .map {
        case Seq(a) => { stringToPrio(a.toString()) }
      }
    intersections.sum
  }

}
