object Day4 {
  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  type CharLocation = (Int, Int, Char)

  def getRequiredCharLocations (x: Int, y: Int, rotation: Int): List[CharLocation] =
    val (xChange, yChange) = rotation match
      case 0 => (1, 0) // left to right
      case 45 => (1, 1) // diagonal to bottom right
      case 90 => (0, 1) // top to bottom
      case 135 => (-1, 1) // diagonal to bottom left 
      case 180 => (-1, 0) // right to left
      case 225 => (-1, -1) // diagonal to top left
      case 270 => (0, -1) // bottom to top
      case 315 => (1, -1) // diagonal to top right
      case _ => (0, 0) // error

    List(
      (x + xChange, y + yChange, 'M'),
      (x + xChange * 2, y + yChange * 2, 'A'),
      (x + xChange * 3, y + yChange * 3, 'S'),
    )

  def parse(lines: List[String]): Set[CharLocation] =
    lines
      .zip(Range(0, lines.length))
      .map((line, y) =>
        line.toList
          .zip(Range(0, line.length))
          .map((c, x) => (x, y, c))
      )
      .flatten
      .toSet
  
  def part1(lines: List[String]): String =
    val locations = parse(lines)

    val Xs = locations.filter((_, _, c) => c == 'X')

    val answer = Range(0, 360, 45).map(rotation =>
      Xs.count((X_x, X_y, _) =>
        getRequiredCharLocations(X_x, X_y, rotation).forall(locations.contains)
      )
    ).sum

    answer.toString()

  def part2(lines: List[String]): String =
    val locations = parse(lines)

    val As = locations.filter((_, _, c) => c == 'A')

    As.count((x, y, _) =>
      val toBottomRight = List((x + 1, y + 1, 'S'), (x - 1, y - 1, 'M')).forall(locations.contains)
      val toTopLeft = List((x + 1, y + 1, 'M'), (x -1, y - 1, 'S')).forall(locations.contains)
      
      val toBottomLeft = List((x - 1, y + 1, 'S'), (x + 1, y - 1, 'M')).forall(locations.contains)
      val toTopRight = List((x - 1, y + 1, 'M'), (x + 1, y - 1, 'S')).forall(locations.contains)

      (toBottomRight || toTopLeft) && (toBottomLeft || toTopRight)
    ).toString()
}
