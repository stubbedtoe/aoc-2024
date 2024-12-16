package Aoc2024

import scala.annotation.tailrec

object Day16 {
  type Location = (Int, Int)
  enum Orientation:
    case North, South, East, West
  
  enum MapItem:
    case Wall, Empty


  case class Branch(
    location: Location,
    orientation: Orientation,
    cost: Long,
    visited: Set[Location]
  )

  def move(location: Location, orientation: Orientation): Location =
    val (x, y) = location
    orientation match
      case Orientation.East => (x + 1, y)
      case Orientation.North => (x, y - 1)
      case Orientation.South => (x, y + 1)
      case Orientation.West => (x - 1, y)

  case class State(
    maze: Map[Location, MapItem],
    location: Location,
    orientation: Orientation,
    currentCost: Long,
    bestCost: Long,
    endLocation: Location,
    branches: List[Branch],
    visited: Set[Location],
  ):
    @tailrec
    final def step(): State =
      val inCurrentDirection = move(location, orientation)
      if (inCurrentDirection == endLocation) then
        val cost = currentCost + 1L
        // println(s"\nend cost: $cost ${if cost < bestCost then "(new best)" else "(not the best)"}\n")
        branches match
          case head::tail =>
            this.copy(
              location = head.location,
              orientation = head.orientation,
              currentCost = head.cost,
              branches = tail,
              bestCost = if cost < bestCost then cost else bestCost,
              visited = head.visited,
            ).step()
          case _ =>
            // no more branches so we're finished
            this
      else
        maze(inCurrentDirection) match
          case MapItem.Empty =>
            if ((currentCost + 1L > bestCost) || visited.contains(inCurrentDirection)) {
              branches match
                case head::tail =>
                  // println(s"changing direction to ${head.orientation} at ${head.location} (cost: ${head.cost})")
                  this.copy(
                    location = head.location,
                    orientation = head.orientation,
                    currentCost = head.cost,
                    branches = tail,
                    visited = head.visited,
                  ).step()
                case _ =>
                  // no more branches so we're finished
                  this
            } else {
              // println(s"stepping $orientation to $inCurrentDirection")
              this.copy(
                location = inCurrentDirection,
                currentCost = currentCost + 1L,
                visited = visited.incl(inCurrentDirection)
              ).addBranches().step()
            }
            
          case MapItem.Wall =>
            branches match
              case head::tail =>
                if (head.cost < bestCost) {
                  // println(s"changing direction to ${head.orientation} at ${head.location} (cost: ${head.cost})")
                  this.copy(
                    location = head.location,
                    orientation = head.orientation,
                    currentCost = head.cost,
                    branches = tail,
                    visited = head.visited,
                  ).step()
                } else {
                  this.copy(branches = tail).step()
                }
                
              case _ =>
                // no more branches so we're finished
                this
      
    end step
    def addBranches(): State =
      val orientations = List(
        Orientation.North,
        Orientation.South,
        Orientation.East,
        Orientation.West
        ).filterNot(o => o == orientation || o == oppositeDirection(orientation))
      val newBranches: List[Branch] =
        orientations.foldLeft(branches)((branches, o) =>
          maze(move(location, o)) match
            case MapItem.Empty =>
              val branchCost = currentCost + 1000L
              val branchesAtHigherCost = branches.filter(b =>
                b.location == location && b.orientation == o && b.cost > branchCost
              )

              val branchesAtLowerCost = branches.filter(b =>
                b.location == location && b.orientation == o && b.cost < branchCost
              )

              (branchesAtHigherCost.size, branchesAtLowerCost.size) match
                case (0, 0) =>
                  // println(s"adding a new branch at $location facing $o")
                  Branch(
                    location = location,
                    orientation = o,
                    cost = branchCost,
                    visited = visited,
                  )::branches
                case (_, 0) =>
                  // println(s"new lower cost for branch at $location facing $o")
                  Branch(
                    location = location,
                    orientation = o,
                    cost = branchCost,
                    visited = visited,
                  )::(branches.toSet.diff(branchesAtHigherCost.toSet).toList)
                case _ =>
                  // println(s"not adding a more expensive branch at $location facing $o")
                  branches

            case MapItem.Wall =>
              branches
        )

      this.copy(branches = newBranches)
    end addBranches
  end State

  def oppositeDirection(orientation: Orientation): Orientation =
    orientation match
      case Orientation.East => Orientation.West
      case Orientation.North => Orientation.South
      case Orientation.West => Orientation.East
      case Orientation.South => Orientation.North

  def initialise(lines: List[String]): State =
    var endLocation = (0, 0)
    var startLocation = (0, 0)
    val maze = 
      lines.zipWithIndex.map(
        (line, y) =>
          line.toCharArray().zipWithIndex.map((c, x) =>
            val location = (x, y)
            val item =
              c match
                case '#' => MapItem.Wall
                case '.' => MapItem.Empty
                case 'S' => startLocation = location; MapItem.Empty
                case 'E' => endLocation = location; MapItem.Empty
            (location, item)
          )).flatten.toMap

    State(
      maze = maze,
      location = startLocation,
      orientation = Orientation.East,
      currentCost = 0L,
      bestCost = Long.MaxValue,
      endLocation = endLocation,
      branches = List.empty,
      visited = Set.empty,
    )


  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def part1(lines: List[String]): String =
    val state = initialise(lines)
    state.addBranches().step().bestCost.toString()

  def part2(lines: List[String]): String =
    "Haven't solved part 2 yet"
}
