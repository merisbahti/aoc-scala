package year2020

object Day12 extends App {

  lazy val lineRe = "(\\w)(\\d+)".r

  def parseInstruction(line: String): (String, Int) = {
    line match {
      case lineRe(instruction, value) => (instruction, value.toInt)
    }
  }

  def move(state: State, direction: Int, amp: Int): State =
    direction match {
      case 0 => state.copy(y = state.y + amp)
      case 90 => state.copy(x = state.x + amp)
      case 180 => state.copy(y = state.y - amp)
      case 270 => state.copy(x = state.x - amp)
    }

  def rotate(state: State, amp: Int): State =
    state.copy(direction = (state.direction + 360 + amp) % 360)

  case class State(direction: Int, x: Int, y: Int)

  def sol1(input: String): Int = {
    val instructions = input.split("\n").toSeq.map(parseInstruction)
    val finalState = instructions.foldLeft(State(90, 0, 0)) {
      case (state, instruction) => {
        val newState = instruction match {
          case ("N", amp) => move(state, 0, amp)
          case ("E", amp) => move(state, 90, amp)
          case ("S", amp) => move(state, 180, amp)
          case ("W", amp) => move(state, 270, amp)
          case ("F", amp) => move(state, state.direction, amp)
          case ("R", amp) => rotate(state, amp)
          case ("L", amp) => rotate(state, -amp)
        }
        newState
      }
    }
    finalState.x.abs + finalState.y.abs
  }

  case class Sol2State(x: Int, y: Int, waypoint: (Int, Int))

  def move2(state: Sol2State, amp: Int): Sol2State = state match {
    case Sol2State(x, y, (wpX, wpY)) => {
      state.copy(x = x + wpX * amp, y = y + wpY * amp)
    }
  }

  // 10 units east and 4 units north of the ship
  // 90 -> 4 units east and 10 units south of the ship
  def rotate2(state: Sol2State, deg: Int): Sol2State =
    deg match {
      case 0 => state
      case 90 =>
        state.copy(waypoint = (state.waypoint._2, -1 * state.waypoint._1))
      case 180 => rotate2(rotate2(state, 90), 90)
      case 270 => rotate2(rotate2(state, 180), 90)
    }

  def sol2(input: String): Int = {
    val instructions = input.split("\n").toSeq.map(parseInstruction)
    val finalState = instructions.foldLeft(Sol2State(0, 0, (10, 1))) {
      case (state, instruction) => {
        val newState = instruction match {
          case ("N", amp) =>
            state.copy(waypoint = (state.waypoint._1, state.waypoint._2 + amp))
          case ("E", amp) =>
            state.copy(waypoint = (state.waypoint._1 + amp, state.waypoint._2))
          case ("S", amp) =>
            state.copy(waypoint = (state.waypoint._1, state.waypoint._2 - amp))
          case ("W", amp) =>
            state.copy(waypoint = (state.waypoint._1 - amp, state.waypoint._2))
          case ("F", amp) => move2(state, amp)
          case ("R", amp) => rotate2(state, amp)
          case ("L", amp) => rotate2(state, 360 - amp)
        }
        newState
      }
    }

    finalState.x.abs + finalState.y.abs

  }

}
