import scala.annotation.tailrec
enum Orientation:
  case Up, Down, Left, Right

enum Cell:
  case Empty, Obstruction

case class Position(x: Int, y: Int)

case class AreaMap(cells: Map[Position, Cell], width: Int, height: Int)

case class State(visited: Set[(Position, Orientation)], position: Position, orientation: Orientation)

case class Result(visited: Set[(Position, Orientation)], looped: Boolean)

def initialise(lines: List[String]): (AreaMap, State) =
  val width = lines.head.length
  val height = lines.length
  val cells = 
    Map.from(lines
      .zipWithIndex
      .map((line, y) =>
        line
          .zipWithIndex
          .map((c, x) =>
            val position = Position(x, y)
            val cell =
              c match
                case '#' =>
                  Cell.Obstruction
                case _ =>
                  Cell.Empty
            (position, cell)
          ).toList
        ).toList.flatten)

  val areaMap = AreaMap(cells, width, height)

  val charArray = lines.flatten.mkString.toCharArray()

  val start = charArray.find(c => c == '^' || c == 'v' || c == '<' || c == '>')
  val orientation = start match
    case Some('^') =>
      Orientation.Up
    case Some('v') =>
      Orientation.Down
    case Some('<') =>
      Orientation.Left
    case Some('>') =>
      Orientation.Right
    case _ =>
      throw new Exception("No start found")
  
  val position = start match
    case Some(c) =>
      val i = charArray.indexOf(c)
      Position(i % width, Math.floor(i / width).toInt)
    case _ =>
      throw new Exception("No start found") 
  
  val state = State(Set((position, orientation)), position, orientation)
  (areaMap, state)


def turn(orientation: Orientation) = 
  orientation match
    case Orientation.Up => Orientation.Right
    case Orientation.Right => Orientation.Down
    case Orientation.Down => Orientation.Left
    case Orientation.Left => Orientation.Up

def move(position: Position, orientation: Orientation) =
  orientation match
    case Orientation.Up =>
      Position(position.x, position.y - 1)
    case Orientation.Down =>
      Position(position.x, position.y + 1)
    case Orientation.Left =>
      Position(position.x - 1, position.y)
    case Orientation.Right =>
      Position(position.x + 1, position.y)

@tailrec
def iterate(areaMap: AreaMap, state: State): Result =
  if (state.visited.size > 1 && state.visited.contains((state.position, state.orientation))) then
    Result(state.visited, true)
  else
    val nextPosition = move(state.position, state.orientation)
    
    areaMap.cells.get(nextPosition) match
      case Some(Cell.Empty)=>
        val newState = State(state.visited + ((state.position, state.orientation)), nextPosition, state.orientation)
        iterate(areaMap, newState)
      case Some(Cell.Obstruction) =>
        val newOrientation = turn(state.orientation)
        val newState = State(state.visited + ((state.position, state.orientation)), state.position, newOrientation)
        iterate(areaMap, newState)
      case None =>
        Result(state.visited + ((state.position, state.orientation)), false)

def willCauseLoop(areaMap: AreaMap, state: State, obstructionLocation: Position): Boolean =
  val withObstruction = AreaMap(areaMap.cells.updated(obstructionLocation, Cell.Obstruction), areaMap.width, areaMap.height)
  iterate(withObstruction, state).looped

object Day6 {
  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def part1(lines: List[String]): String =
    val (areaMap, state) = initialise(lines)
    val visited = iterate(areaMap, state).visited.map(_._1).toSet
    visited.size.toString()

  def part2(lines: List[String]): String =
    val (areaMap, state) = initialise(lines)
    val visited = iterate(areaMap, state).visited.map(_._1).toSet
    val obstructionLocations = visited.filter(position => state.position != position)
    obstructionLocations
      .count(obstruction => willCauseLoop(areaMap, state, obstruction))
      .toString()
}
