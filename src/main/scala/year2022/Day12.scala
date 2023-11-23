package year2022

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

  def getPossibleSteps(
      map: HeightMap,
      position: Position,
      visitedNodes: Seq[Position]
  ): Seq[Position] = {
    val (x, y) = position

    val currentChar = getChar(map, position)
    val neighbors: Seq[Position] = for {
      xDiff <- Seq(-1, 0, 1)
      yDiff <- Seq(-1, 0, 1)
      if (xDiff == 0 || yDiff == 0)
      position = (x + xDiff, y + yDiff)
      possibleChar = getChar(map, position)
      if (possibleChar.exists(possible =>
        currentChar.exists(curr =>
          possible.codePointAt(0) == curr.codePointAt(0) + 1
            || possible.codePointAt(0) == curr.codePointAt(0)
        )
      ) || (currentChar == Some("S") && possibleChar == Some("a"))
        || (currentChar == Some("z") && possibleChar == Some("E")))
      if (position != (x, y))
    } yield position
    neighbors.filter(pos => !visitedNodes.contains(pos))
  }

  def renderPathTaken(
      map: HeightMap,
      pathTaken: Seq[Position]
  ): Seq[Seq[String]] = {
    if (pathTaken.length == 0) { return map }
    val (x, y) = pathTaken.head
    val nextPosition = pathTaken.tail.headOption
    val icon: String =
      if (pathTaken.length == 1) "E"
      else
        nextPosition match {
          case Some((nextX, nextY)) =>
            if (nextX == x && y > nextY)
              "^"
            if (nextX == x && y < nextY) {
              "v"
            } else if (nextY == y && nextX < x) {
              "<"
            } else if (nextY == y && nextX > x) {
              ">"
            } else {
              "^"
            }
          case None => "E"
        }
    val newMap = map.updated(y, map(y).updated(x, icon))
    return renderPathTaken(newMap, pathTaken.tail)
  }

  def printPathTaken(
      map: HeightMap,
      pathTaken: Seq[Position]
  ): Unit = {
    val renderedPath = renderPathTaken(map, pathTaken)
    for (row <- renderedPath) {
      println(row.mkString(""))
    }

  }

  def recurse(map: HeightMap, pathTaken: Seq[Position]): Seq[Seq[Position]] = {
    val position = pathTaken.last
    val char = getChar(map, position)
    if (char == Some("E")) {
      return Seq(pathTaken)
    }
    val options = getPossibleSteps(map, position, pathTaken)
    val rv = options.flatMap(option => {
      recurse(map, pathTaken.appended(option))
    })
    rv
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
    println("starting point is", startingPoint)
    val a = startingPoint.get._2
    val res = recurse(map, Seq(a))
    for (path <- res) {
      printPathTaken(map, path)
    }
    println(s"""recurse done found paths: ${res.length}""")
    val min = res.minBy(_.size)

    println(s"""paths with min size ${res.filter(_.size == min.size).length}""")

    for (path <- res.filter(_.size == min.size)) {
      printPathTaken(map, path)
      println("----")

    }

    printPathTaken(map, min)
    return min.length - 1

  }

  def sol2(input: String): Int = ???

}
