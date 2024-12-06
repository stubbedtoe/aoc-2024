import scala.annotation.tailrec
enum Orientation:
  case Up, Down, Left, Right

enum Cell:
  case Empty, Obstruction

def turn(orientation: Orientation) = 
  orientation match
    case Orientation.Up => Orientation.Right
    case Orientation.Right => Orientation.Down
    case Orientation.Down => Orientation.Left
    case Orientation.Left => Orientation.Up

def move(position: (Int, Int), orientation: Orientation) =
  orientation match
    case Orientation.Up =>
      (position._1, position._2 - 1)
    case Orientation.Down =>
      (position._1, position._2 + 1)
    case Orientation.Left =>
      (position._1 - 1, position._2)
    case Orientation.Right =>
      (position._1 + 1, position._2)

class MapOfArea(lines: List[String]):
  var position: (Int, Int) = (0, 0)
  private var orientation: Orientation = Orientation.Up
  private var cells = Array[(Int, Int, Cell)]()
  var bounds: (Int, Int) = (0, 0)
  var visited = Set[(Int, Int, Orientation)]()
  private var initialPosition = (0, 0)
  private var initialOrientation = Orientation.Up

  def apply() =
    lines
      .zipWithIndex
      .foreach((line, y) =>
        line
          .zipWithIndex
          .foreach((c, x) =>
            c match
              case '^' | 'v' | '<' | '>' =>
                position = (x, y)
                orientation = c match
                  case '^' => Orientation.Up
                  case 'v' => Orientation.Down
                  case '<' => Orientation.Left
                  case '>' => Orientation.Right
                cells = cells :+ (x, y, Cell.Empty)
                visited = visited + ((x, y, orientation))
              case '#' =>
                cells = cells :+ (x, y, Cell.Obstruction)
              case '.' =>
                cells = cells :+ (x, y, Cell.Empty)
          )
      )
      val height = lines.length
      val width = lines.head.length
      bounds = (width, height)
      initialPosition = position
      initialOrientation = orientation

  def getCell(positionToGet: (Int, Int)): Option[Cell] =
    val (x, y) = positionToGet
    if x < 0 || x >= bounds._1 || y < 0 || y >= bounds._2 then
      None
    else
      val (_, _, cell) = cells(x + y * bounds._1)
      Some(cell)

  @tailrec
  final def iterate(part: Part, iteration: Int = 0): Boolean = // true if there's a loop
    if (iteration > 0 && visited.contains((position._1, position._2, orientation)) && part == Part.Two) then
      true
    else
      visited = visited + ((position._1, position._2, orientation))
      
      val nextPosition = move(position, orientation)
      
      val cell = getCell(nextPosition)
      cell match
        case Some(Cell.Empty)=>
          position = nextPosition
          iterate(part, iteration + 1)
        case Some(Cell.Obstruction) =>
          orientation = turn(orientation)
          iterate(part, iteration + 1)
        case None =>
          false

  def reset() =
    visited = Set((initialPosition._1, initialPosition._2, initialOrientation))
    position = initialPosition
    orientation = initialOrientation

  def willCauseLoop(obstructionLocation: (Int, Int)) =
    val (x, y) = obstructionLocation
    cells = cells.updated(x + y * bounds._1, (x, y, Cell.Obstruction))
    val causedLoop = iterate(Part.Two)
    cells = cells.updated(x + y * bounds._1, (x, y, Cell.Empty))
    reset()
    causedLoop


object Day6 {
  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def part1(lines: List[String]): String =
    val map = MapOfArea(lines)
    map()
    map.iterate(Part.One)
    map.visited.map((x, y, _) => (x, y)).toSet.size.toString()

  def part2(lines: List[String]): String =
    val map = MapOfArea(lines)
    map()
    map.iterate(Part.One)
    val visited = map.visited
    map.reset()
    val locations = visited.map((x, y, _) => (x, y)).filter(loc => loc != map.position)
    
    locations.count(map.willCauseLoop).toString()
}
