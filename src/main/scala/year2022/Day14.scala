package year2022

import java.text.ParseException

object Day14 extends App {

  sealed trait MapNode
  case class Rock() extends MapNode
  case class Sand() extends MapNode
  type CaveMap = Map[(Int, Int), MapNode]

  def parsePath(input: String): Seq[(Int, Int)] = {
    val segmentStrings = input.split(" -> ").toList
    val segments = segmentStrings.map { p =>
      val coords = p.split(",").toList
      (coords(0).toInt, coords(1).toInt)
    }

    return segments(0) +: (0 to segments.length).flatMap { i =>
      val p1 = segments.lift(i)
      val p2 = segments.lift(i + 1)
      val res = for {
        (x1, y1) <- p1
        (x2, y2) <- p2
        path =
          if (x1 == x2) {
            (y1 to y2 by (if (y1 > y2) -1 else 1)).drop(1).map(y => (x1, y))
          } else if (y1 == y2) {
            (x1 to x2 by (if (x1 > x2) -1 else 1)).drop(1).map(x => (x, y1))
          } else {
            throw new ParseException(
              s"Invalid path from: ($x1, $y1) to ($x2, $y2)",
              0
            )
          }
      } yield {
        path
      }
      res.getOrElse(Seq.empty)
    }
  }

  def parse(input: String): CaveMap = {
    val sandpoints = input.split("\n").flatMap(parsePath).toSeq
    sandpoints.foldLeft(Map.empty[(Int, Int), MapNode]) { (acc, p) =>
      acc + (p -> Rock())
    }
  }

  def printCaveMap(caveMap: CaveMap): String = {
    val minX = caveMap.keys.map(_._1).min
    val maxX = caveMap.keys.map(_._1).max
    val minY = caveMap.keys.map(_._2).min
    val maxY = caveMap.keys.map(_._2).max

    (minY to maxY)
      .map { y =>
        (minX to maxX)
          .map { x =>
            val node = caveMap.get((x, y))
            (node match {
              case Some(Rock()) => "#"
              case Some(Sand()) => "o"
              case None         => "."
            })
          }
          .mkString("")
      }
      .mkString("\n")
  }

  def sol1(input: String): Int = {
    -1
  }
  def sol2(input: String): Int = ???

}
