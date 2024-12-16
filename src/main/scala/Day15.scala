import scala.annotation.tailrec

object Day15 {
  type Location = (Int, Int)
  enum Item:
    case Wall, Box, Empty, BoxLeft, BoxRight

  def doMove(move: Move, location: Location): Location =
    val (x, y) = location
    move match
      case Move.Left => (x - 1, y)
      case Move.Down => (x, y + 1)
      case Move.Up => (x, y - 1)
      case Move.Right => (x + 1, y)
    

  case class State(room: Map[Location, Item], robot: Location):
    @tailrec
    final def gatherLocationsForMove(at: List[Location], to: Move, locations: List[List[Location]]): List[List[Location]] =
      val items = at.map(room(_))
      (to, items) match
        case (_, things) if things.forall(_ == Item.Empty) =>
          at::locations
        case (_, things) if things.exists(_ == Item.Wall) =>
          List.empty
        case (m, things) if things.exists(_ == Item.Box) || m == Move.Left || m == Move.Right =>
          gatherLocationsForMove(
            at = at.map(doMove(to, _)),
            to = to,
            locations = at::locations)
        case (_, _) => // up or down when pushing a half box
          val nextAt = at.foldLeft(at.toSet)((s, location) =>
            room(location) match
              case Item.BoxLeft =>
                s.incl(doMove(Move.Right, location))
              case Item.BoxRight =>
                s.incl(doMove(Move.Left, location))
              case _ =>
                s
          ).toList
          val nextAtFiltered = nextAt.filter(location =>
            (room(location), room(doMove(to, location))) match
              case (Item.Empty, Item.Empty) =>
                false
              case _ =>
                true 
          )
          // println(s"${nextAtFiltered.length} in the nextAt")
          gatherLocationsForMove(
            at = nextAtFiltered.map(doMove(to, _)),
            to = to,
            locations = nextAt::locations)

    @tailrec
    final def performMoves(locations: List[List[Location]], move: Move): State =
      locations match
        case Nil =>
          this
        case last::Nil =>
          this.copy(
            room = last.foldLeft(room)((r, location) => r.updated(location, Item.Empty)) 
          )
        case secondLast::last::Nil =>
          this.copy(
            room = secondLast.foldLeft(room)((r, location) => r.updated(location, Item.Empty)),
            robot = secondLast.last,
          ).performMoves(List(last), move)
        case loc1::loc2::rest =>
          if (move == Move.Left || move == Move.Right)
            this.copy(
              room = loc1.zip(loc2).foldLeft(room)((r, location) =>
                r.updated(location._1, r(location._2)).updated(location._2, Item.Empty))
            ).performMoves((loc2::rest), move)
          else
            val moves = 
              for 
                from <- loc1
                to <- loc2 if to._1 == from._1
              yield (from, to)
            this.copy(
              room = moves.foldLeft(room)((r, locations) =>
                r.updated(locations._1, r(locations._2)).updated(locations._2, Item.Empty))
            ).performMoves(loc2::rest, move)

    def printState(height: Int) =
      Range(0, height).foreach(y =>
        val line = room.toList.filter((l, _) => l._2 == y)
        val s = Range(0, line.length)
          .map(x =>
            if ((x, y) == robot)
              '@'
            else
              room((x, y)) match
                case Item.Empty =>
                  '.'
                case Item.Wall =>
                  '#'
                case Item.BoxLeft =>
                  '['
                case Item.BoxRight =>
                  ']'
                case _ => '_'
            
          ).mkString
        println(s)
      )

  end State
        
      
      

  enum Move:
    case Up, Down, Left, Right

  def parse(lines: List[String], part: Part): (State, List[Move]) =
    val emptyLine = lines.indexOf("")
    val (roomLines, moveLines) = lines.splitAt(emptyLine)
    val width = roomLines.head.length
    val height = roomLines.length
    var robot = (0,0)

    val room =
      roomLines
        .zipWithIndex
        .map(
          (line, y) =>
            line
              .toCharArray()
              .zipWithIndex
              .map((c, x) =>
                val location = (x, y)
                if part == Part.One then
                  c match
                    case '#' => List((location, Item.Wall))
                    case 'O' => List((location, Item.Box))
                    case '@' =>
                      robot = location
                      List((location, Item.Empty))
                    case _ => List((location, Item.Empty))
                else
                  val left = (x * 2, y)
                  val right = ((x * 2) + 1, y)
                  c match
                    case '#' => List((left, Item.Wall), (right, Item.Wall))
                    case 'O' => List((left, Item.BoxLeft), (right, Item.BoxRight))
                    case '@' =>
                      robot = left
                      List((left, Item.Empty), (right, Item.Empty))
                    case _ => List((left, Item.Empty), (right, Item.Empty))
              ).flatten
          ).flatten
        .toMap
    
    val moves = moveLines.reduce(_ ++ _).toCharArray().map(c =>
      c match
        case '<' => Move.Left
        case '>' => Move.Right
        case '^' => Move.Up
        case 'v' => Move.Down
    ).toList

    (State(room = room, robot = robot), moves)


  def solve(lines: List[String], part: Part): String =
    val test = """#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^""".split("\n").toList

    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines) // or test
    }

  def part1(lines: List[String]): String =
    val (state, moves) = parse(lines, Part.One)
    
    val afterMoves =
      moves.foldLeft(state)((s, m) =>
        val locations = s.gatherLocationsForMove(at = List(doMove(m, s.robot)), to = m, locations = List(List(s.robot)))
        s.performMoves(locations, m)
      )

    afterMoves
      .room
      .toList
      .filter((_, item) => item == Item.Box)
      .map((l, _) =>
        val (x, y) = l 
        (100 * y) + x)
      .sum
      .toString()

  def part2(lines: List[String]): String =
    var (state, moves) = parse(lines, Part.Two)

    val afterMoves = 
      moves.foldLeft(state)((s, m) =>
        val calulatedAt =
          val next = doMove(m, s.robot)
          (s.room(next), m) match
            case (Item.BoxLeft, direction) if direction == Move.Up || direction == Move.Down =>
              List(doMove(Move.Right, next), next)
            case (Item.BoxRight, direction) if direction == Move.Up || direction == Move.Down =>
              List(doMove(Move.Left, next), next)
            case _ =>
              List(next)
          
        val locations = s.gatherLocationsForMove(at = calulatedAt, to = m, locations = List(List(s.robot)))
        s.performMoves(locations, m)
      )

    // afterMoves.printState(lines.length)

    afterMoves
      .room
      .toList
      .filter((_, item) => item == Item.BoxLeft)
      .map((l, _) =>
        val (x, y) = l 
        (100 * y) + x)
      .sum
      .toString()

  end part2
}
