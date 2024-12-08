object Day8 {
  case class Location(x: Int, y: Int)
  type Antenna = (Location, Char)
  case class ParseResult(pairs: Seq[(Location, Location)], width: Int, height: Int)
  
  def parseLines(lines: List[String]): Array[Antenna] =
    lines
      .zipWithIndex
        .map((line, y) =>
          line
            .toCharArray
            .zipWithIndex
            .filter(_._1 != '.' )
            .map((c, x) =>
              (Location(x, y), c)
      )
    ).flatten.toArray

  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def getPairsOfAnntennas(lines: List[String]): ParseResult =
    val width = lines.head.length
    val height = lines.length
    val antennas = parseLines(lines)

    val pairs = for {
      (l1, c1) <- antennas
      (l2, c2) <- antennas
      if preceeds(l1, l2) && c1 == c2
    } yield (l1, l2)
    ParseResult(pairs, width, height)

  def preceeds(l1: Location, l2: Location): Boolean =
    l1.y < l2.y || (l1.y == l2.y && l1.x < l2.x)

  def moveBy(l1: Location, diff: Location): Location =
    Location(l1.x + diff.x, l1.y + diff.y)

  def inLine(l1: Location, l2: Location, test: Location): Boolean =
    val diff = Location(l1.x - l2.x, l1.y - l2.y)
    val testDiff = Location(l1.x - test.x, l1.y - test.y)
    diff.x * testDiff.y == diff.y * testDiff.x

  def part1(lines: List[String]): String =
    val parsed = getPairsOfAnntennas(lines)
    val antinodeLocations = 
      parsed.pairs
        .map((l1, l2) => 
          val diffOne = Location(l1.x - l2.x, l1.y - l2.y)
          val antiNode1 = moveBy(l1, diffOne)
          val diffTwo = Location(l2.x - l1.x, l2.y - l1.y)
          val antiNode2 = moveBy(l2, diffTwo)
          List(antiNode1, antiNode2)
            .filter((l) => l.x >= 0 && l.x < parsed.width && l.y >= 0 && l.y < parsed.height)
        ).flatten
        .toSet

    antinodeLocations.size.toString() 

  def part2(lines: List[String]): String =
    val parsed = getPairsOfAnntennas(lines)
    val antinodeLocations = 
      parsed.pairs
        .map((l1, l2) => 
          for {
            x <- Range(0, parsed.width)
            y <- Range(0, parsed.height)
            if inLine(l1, l2, Location(x, y))
          } yield Location(x, y)
        ).flatten
        .toSet

    antinodeLocations.size.toString()
}
