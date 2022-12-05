package year2020

sealed abstract class Seat
case class Empty() extends Seat
case class Occupied() extends Seat
case class Floor() extends Seat

object Day11 extends App {
  type Seats = Array[Array[Seat]]

  // make this lazier to make it more efficient
  def compareSeats(a: Seats, b: Seats): Boolean = (for (
    (rows, y) <- a.zipWithIndex;
    (seatA, x) <- rows.zipWithIndex;
    seatB = b.lift(y).flatMap(_.lift(x))
  ) yield (seatB.contains(seatA))).find(_ == false).isEmpty

  def parse(input: String): Array[Array[Seat]] = {
    val a: Seat = Empty()
    val lines = input.split("\n")
    val stuff = lines.map(_.split(""))
    stuff
      .map(_.map {
        case "." => Floor()
        case "#" => Occupied()
        case "L" => Empty()
        case c   => throw new Exception(s"Invalid character in input: $c")
      })
  }

  def getAdjacentOccupied(
      x: Integer,
      y: Integer,
      input: Array[Array[Seat]]
  ): Integer = {
    val adjacentSeats =
      for (
        dx <- -1 to 1;
        dy <- -1 to 1;
        if dx != 0 || dy != 0
      )
        yield (input
          .lift(y + dy)
          .flatMap(_.lift(x + dx)))

    adjacentSeats.count(_ == Some(Occupied()))
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

    // maybe think about infinite loops here?
    while (!equal) {
      val oldSeats = seats;
      seats = iter(seats)
      equal = compareSeats(oldSeats, seats)
    }

    val occupiedCount = seats.flatten.count(_ == Occupied())

    occupiedCount
  }
}
