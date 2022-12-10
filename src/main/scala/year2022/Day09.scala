package year2022

object Day09 extends App {

  case class Point(x: Int, y: Int)

  private def visualize(rope: Seq[Point]) = {
    val minX = -10 // orrope.minBy(_.x).x
    val maxX = 10 // || rope.maxBy(_.x).x

    val minY = -10 // or rope.minBy(_.y).y
    val maxY = 10 // or rope.maxBy(_.y).y

    (minY to maxY)
      .map(y => {
        (minX to maxX)
          .map(x => {
            rope.zipWithIndex
              .find {
                case (Point(pX, pY), index) => {
                  pX == x && pY == y
                }
              }
              .map(x => x._2.toString)
              .getOrElse(".")
          })
          .mkString("")
      })
      .reverse
      .mkString("\n")
  }

  private def moveHead(move: String, head: Point): Point = {
    move match {
      case "U" => head.copy(y = head.y + 1)
      case "D" => head.copy(y = head.y - 1)
      case "L" => head.copy(x = head.x - 1)
      case "R" => head.copy(x = head.x + 1)
    }
  }

  def moveTail(tail: Point, head: Point): Point = {
    val shouldMoveX = (tail.x - head.x).abs == 2
    val shouldMoveY = (tail.y - head.y).abs == 2
    val diffColumnAndRow = tail.x != head.x && tail.y != head.y
    val xDiff =
      if ((diffColumnAndRow && shouldMoveY) || shouldMoveX)
        (head.x - tail.x).sign
      else 0
    val yDiff =
      if ((diffColumnAndRow && shouldMoveX) || shouldMoveY)
        (head.y - tail.y).sign
      else 0
    tail.copy(tail.x + xDiff, tail.y + yDiff)
  }

  private def parseInput(input: String) = {
    val regex = "([ULRD]) (\\d+)".r
    input
      .split("\n")
      .flatMap {
        case regex(dir, count) =>
          (0 until count.toInt).map(_ => dir)
        case s => throw new Exception(s"$s did not match regex")
      }
  }

  def sol1(input: String): Int = generalSol(2)(input)

  case class StateSol2(rope: Seq[Point], tailVisits: Seq[Point])

  private def generalSol(ropeSize: Int)(input: String) = {
    val parsed = parseInput(input)

    val initialRope = (0 until ropeSize).map(_ => Point(0, 0))

    val finalState =
      parsed.foldLeft(StateSol2(initialRope, tailVisits = Seq(Point(0, 0)))) {
        case (StateSol2(oldRope, tailVisits), move) =>
          val newRope = oldRope.zipWithIndex.foldLeft(Seq[Point]()) {
            case (newRope, (point, index)) =>
              newRope
                .lift(index - 1)
                .map(currHead => {
                  newRope.appended(moveTail(point, currHead))
                })
                .getOrElse(
                  Seq(moveHead(move, point))
                )
          }
          val last = newRope.last

          val newState = StateSol2(newRope, tailVisits.appended(newRope.last))
          val vis = visualize(newState.rope)
          newState
      }
    finalState.tailVisits.toSet.size

  }

  def sol2(input: String): Int = generalSol(10)(input)

}
