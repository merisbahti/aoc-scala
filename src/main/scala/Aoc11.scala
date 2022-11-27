sealed abstract class Seat
case class Empty() extends Seat
case class Occupied() extends Seat
case class Floor() extends Seat

object Aoc11 extends App {
  type Seats = Array[Array[Seat]]
  def compareSeats(a: Seats, b: Seats): Boolean = (for (
    (rows, y) <- a.zipWithIndex;
    (seatA, x) <- rows.zipWithIndex;
    seatB = b.lift(y).flatMap(_.lift(x))
  ) yield (seatB.contains(seatA))).find(_ == false).isEmpty

  def parse(input: String): Seats = {
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

  def nextSeatState(seat: Seat, adjacentCount: Integer): Seat = seat match {
    case Empty() if adjacentCount == 0    => Occupied()
    case Occupied() if adjacentCount >= 4 => Empty()
    case e                                => e
  }

  def iter(seats: Seats): Seats =
    seats.zipWithIndex.map {
      case (ys, y) => {
        ys.zipWithIndex.map {
          case (xs, x) => {
            val adj = getAdjacentOccupied(x, y, seats);
            nextSeatState(xs, adj)
          }
        }
      }
    }

  def sol(input: String): Integer = {
    val parsed = parse(input);

    var seats = iter(parsed);

    var equal = compareSeats(parsed, seats)
    while (!equal) {
      val oldSeats = seats;
      seats = iter(seats)
      equal = compareSeats(oldSeats, seats)
    }

    val occupiedCount = seats.flatten.count(_ match {
      case Occupied() => true
      case _          => false
    })

    occupiedCount
  }
}
