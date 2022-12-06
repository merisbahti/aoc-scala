package year2020

import scala.util.Try

object Day13 extends App {

  def sol1(input: String): Int = {
    val Array(rawEarliest, rawIds) = input.split("\n")
    val timestamp = rawEarliest.toInt
    val ids =
      rawIds.split(",").map(x => Try(x.toInt)).filter(_.isSuccess).map(_.get)
    val (id, waitingTime) = ids
      .map(id => {
        val waitingTime = ((timestamp % id) - id).abs
        (id, waitingTime)
      })
      .minBy(_._2)
    id * waitingTime
  }

  def sol2(input: String): Long = {
    val Array(_, rawIds) = input.split("\n")
    val idAndOffsets = rawIds.split(",").zipWithIndex.flatMap { case (c, i) =>
      Try((c.toInt, i)).toOption
    }
    var timeStamp = 0
    var allRight = false
    do {
      if (timeStamp % 1000000000 == 0) {
        println(timeStamp)
      }
      allRight = idAndOffsets.forall { case (id, offset) =>
        val isRight = (timeStamp + offset) % id == 0
        isRight
      }
      timeStamp += 1
    } while (!allRight)
    return timeStamp
    val a =
      LazyList
        .from(0)
        .find(timeStamp => {
          if (timeStamp % 100000000 == 0) {
            println(timeStamp)
          }
          idAndOffsets.forall { case (id, offset) =>
            val isRight = (timeStamp + offset) % id == 0
            isRight
          }
        })
    a.get

  }

}
