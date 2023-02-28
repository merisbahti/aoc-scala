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

  def getPossibleSteps(map: HeightMap, position: Position): Seq[Position] = {
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
        )
      ) || (currentChar == Some("S") && possibleChar == Some("a"))
        || (currentChar == Some("z") && possibleChar == Some("E")))
    } yield position
    neighbors
  }

  def recurse(map: HeightMap, pathTaken: Seq[Position]): Seq[Seq[Position]] = {
    val position = pathTaken.last
    val char = getChar(map, position)
    if (char == Some("E")) {
      return Seq(pathTaken)
    }
    val options = getPossibleSteps(map, position)
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
    val a = startingPoint.get._2
    val res = recurse(map, Seq(a))
    return res.size

  }

  def sol2(input: String): Int = ???

}
