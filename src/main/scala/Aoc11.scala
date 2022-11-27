sealed abstract class Seat
case class Empty() extends Seat
case class Occupied() extends Seat
case class Floor() extends Seat

object Aoc11 extends App {
  def parse(input: String): Array[Array[Seat]] = {
    val a: Seat = Empty()
    val lines = input.split("\n")
    val stuff = lines.map(_.split(""))
    stuff
      .map(_.map {
        _ match {
          case "." => Floor()
          case "#" => Occupied()
          case "L" => Empty()
          case c   => throw new Exception(s"Couldn't parse character: $c")
        }
      })
  }

  def getAdjacentOccupied(
      x: Integer,
      y: Integer,
      input: Array[Array[Seat]]
  ): Integer = {
    val adj =
      for (dx <- -1 to 1; dy <- -1 to 1; if dx != 0 || dy != 0)
        yield (dx, dy)
    val mapped =
      adj.map { case (dx, dy) =>
        input
          .lift(y + dy)
          .flatMap(_.lift(x + dx))
      }

    mapped.count(_ match {
      case Some(Occupied()) => true
      case _                => false
    })
  }

  def iter(seat: Seat, adjacentCount: Integer): Seat = seat match {
    case Empty() if adjacentCount == 0    => Occupied()
    case Occupied() if adjacentCount >= 4 => Empty()
    case e                                => e
  }

  def sol(input: String): Integer = {
    val parsed = parse(input);

    val ret = parsed.zipWithIndex.map {
      case (ys, y) => {
        ys.zipWithIndex.map {
          case (xs, x) => {
            val adj = getAdjacentOccupied(x, y, parsed);
            iter(xs, adj)
          }
        }
      }
    }

    val occupiedCount = ret.flatten.count(_ match {
      case Occupied() => true
      case _          => false
    })

    occupiedCount
  }
}
