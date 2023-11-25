package year2022

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

object Day12 {

  type HeightMap = Seq[Seq[String]]
  type Position = (Int, Int)

  def parseMap(input: String): HeightMap = {
    input.split("\n").toSeq.map(_.split("").toSeq)
  }

  def getChar(map: HeightMap, position: Position): Option[String] = {
    val (x, y) = position
    for {
      row <- map.lift(y)
      char <- row.lift(x)
    } yield char
  }

  def getAltitude(char: String): Int = {
    val Pattern = "([a-z])".r
    char match {
      case Pattern(char) => (char.codePointAt(0) - 97)
      case "S"           => ("a".codePointAt(0) - 97)
      case "E"           => ("z".codePointAt(0) - 97)
    }
  }

  def getNeighbors(
      map: HeightMap,
      position: Position
  ): Seq[Position] = {
    val (x, y) = position
    val neighbors: Seq[Position] = for {
      (xDiff, yDiff) <- Seq((0, 1), (-1, 0), (1, 0), (0, -1))
      newPosition: Position = (x + xDiff, y + yDiff)
      isAllowed = for {
        currentChar <- getChar(map, position)
        nextChar <- getChar(map, newPosition)
        diff = getAltitude(nextChar) - getAltitude(currentChar)
      } yield {
        diff <= 1
      }
      if (isAllowed.getOrElse(false))
    } yield newPosition
    neighbors
  }

  case class Node(pos: Position, cost: Int)

  case class Context(visitedNodes: Seq[Node], queuedNodes: Seq[Node])

  def printGraph(baseMap: HeightMap, graph: Map[Position, Int]): Unit = {
    baseMap.zipWithIndex.foreach { case (row, y) =>
      row.zipWithIndex.foreach { case (col, x) =>
        val char = getChar(baseMap, (x, y)).get
        val cost = graph.get((x, y))
        val charToPrint = cost match {
          case Some(cost) => "."
          case None       => char
        }
        print(charToPrint)
      }
      println()
    }
  }

  def buildGraph(
      heightMap: HeightMap,
      startNode: Node,
      endPoint: Position
  ): Map[Position, Int] = {
    var gScore: Map[Position, Int] = Map(startNode.pos -> startNode.cost)
    var openSet: PriorityQueue[Node] =
      PriorityQueue(startNode)(Ordering.by(-_.cost))

    while (openSet.nonEmpty && gScore.get(endPoint).isEmpty) {
      val currNode = openSet.dequeue()

      for (nextPos <- getNeighbors(heightMap, currNode.pos)) {
        val tentativeGscore = currNode.cost + 1
        if (gScore.get(nextPos).getOrElse(Int.MaxValue) >= tentativeGscore) {
          val nextNode = Node(nextPos, tentativeGscore)
          gScore = gScore.updated(nextPos, tentativeGscore)
          if (openSet.find(_.pos == nextPos).isEmpty)
            openSet.enqueue(nextNode)
        }
      }
    }
    printGraph(heightMap, gScore)

    gScore
  }

  def sol1(input: String): Int = {
    val map = parseMap(input)
    val startingPoint =
      map.zipWithIndex
        .map { case (rows, y) =>
          rows.zipWithIndex.map { case (col, x) => (col, (x, y)) }
        }
        .flatten
        .find { case (col, pos) => col == "S" }

    val endPoint =
      map.zipWithIndex
        .map { case (rows, y) =>
          rows.zipWithIndex.map { case (col, x) => (col, (x, y)) }
        }
        .flatten
        .find { case (col, pos) => col == "E" }
        .getOrElse(throw new Error("could not find endpoint"))
    println("starting point is", startingPoint)

    println("getting starting point")
    val a = startingPoint.get._2
    println(s"starting point is $a")
    val graph = buildGraph(map, Node(a, 0), endPoint._2)

    println(s"endPoint is $endPoint")
    val endNode = graph.get(endPoint._2)

    return endNode.getOrElse(-1)

  }

  def sol2(input: String): Int = {

    val map = parseMap(input)
    val startingPoints =
      map.zipWithIndex
        .map { case (rows, y) =>
          rows.zipWithIndex.map { case (col, x) => (col, (x, y)) }
        }
        .flatten
        .filter { case (col, pos) => col == "S" || col == "a" }

    val endPoint =
      map.zipWithIndex
        .map { case (rows, y) =>
          rows.zipWithIndex.map { case (col, x) => (col, (x, y)) }
        }
        .flatten
        .find { case (col, pos) => col == "E" }
        .getOrElse(throw new Error("could not find endpoint"))

    println("getting starting point")
    val alternatives = for {
      startingPoint <- startingPoints
      graph = buildGraph(map, Node(startingPoint._2, 0), endPoint._2)
    } yield (startingPoint._2, graph.get(endPoint._2))

    alternatives.minBy(_._2.getOrElse(Int.MaxValue))._2.getOrElse(-1)

  }

}
