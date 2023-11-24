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
    map.lift(y).flatMap(_.lift(x))
  }

  def getAltitude(char: String): Int = {
    val Pattern = "([a-z])".r
    char match {
      case Pattern(char) => (char.codePointAt(0) - 97)
      case "S"           => ("a".codePointAt(0) - 97)
      case "E"           => ("z".codePointAt(0) - 97)
    }
  }

  def getPossibleSteps(
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
        diff == 0 || diff == 1
      }
      if (isAllowed.getOrElse(false))
    } yield newPosition
    neighbors
  }

  case class Node(pos: Position, cost: Int)

  case class Context(visitedNodes: Seq[Node], queuedNodes: Seq[Node])

  def buildGraph(
      heightMap: HeightMap,
      startNode: Node
  ): Set[Node] = {
    var visitedNodes: Set[Node] = Set()
    var queuedNodes: PriorityQueue[Node] =
      PriorityQueue(startNode)(Ordering.by(-_.cost))

    while (queuedNodes.nonEmpty) {
      val currNode = queuedNodes.dequeue()
      visitedNodes += currNode
      println(s"blah $currNode, visitedNodes ${visitedNodes.size}")

      val neighbors = getPossibleSteps(
        heightMap,
        currNode.pos
      ).map(nextPos => Node(nextPos, currNode.cost + 1))

      val unvisitedNeighbors =
        neighbors.filterNot(node => visitedNodes.map(_.pos).contains(node.pos))
      println(
        s"neighbors ${neighbors.length}, unvisited ${unvisitedNeighbors.length}"
      )

      unvisitedNeighbors.foreach(queuedNodes.enqueue(_))
    }

    visitedNodes
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
    val graph = buildGraph(map, Node(a, 0))

    println(s"endPoint is $endPoint")
    val endNode = graph.find { x => x.pos == endPoint._2 }

    return endNode.map(_.cost).getOrElse(-1)

  }

  def sol2(input: String): Int = ???

}
