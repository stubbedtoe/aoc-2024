package Aoc2024

type Location = (Int, Int)
type TopologicalMap = Map[Location, Int]

object Day10 {
  def parse(lines: List[String]): TopologicalMap =
    Map.from(
        lines
          .zipWithIndex
          .map(
            (line, y) =>
              line
                .toCharArray()
                .zipWithIndex
                .map((c, x) =>
                  ((x, y), c.asDigit)
                )
          ).flatten)

  def areAdjacent(l1: Location, l2: Location): Boolean =
    val (x1, y1) = l1
    val (x2, y2) = l2
    val sameColumn = x1 == x2
    val sameRow = y1 == y2

    (sameColumn && Math.abs(y1 - y2) == 1) || (sameRow && Math.abs(x1 - x2) == 1)

  def trailheadScore(map: TopologicalMap, location: Location): Set[Location] =
    map.get(location) match
      case Some(currentAltitude) =>
        val nextLocations = map.filter((loc, altitude) => altitude == currentAltitude + 1 && areAdjacent(location, loc))
        if (currentAltitude == 8)
          nextLocations.keySet
        else
          nextLocations.map((loc, _) => trailheadScore(map, loc)).flatten.toSet
      case None =>
        Set.empty


  def trailheadRating(map: TopologicalMap, location: Location, previousLocations: List[Location]): Long =
    map.get(location) match
      case Some(currentAltitude) =>
        val nextLocations = map.filter((loc, altitude) => altitude == currentAltitude + 1 && areAdjacent(location, loc))
        if (currentAltitude == 8)
          nextLocations.keySet.size
        else
          nextLocations.map((loc, _) => trailheadRating(map, loc, location::previousLocations)).sum
      case None =>
        0L
    
    
  def solve(lines: List[String], part: Part): String =
    // for every 0-height location, construct a set of 9-locations reachable from the start location
    // for every search location, construct a set from the 9-locations reachable in each direction

    val parsed = parse(lines)
    val trailheads = parsed.filter((_, altitude) => altitude == 0)
    part match
      case Part.One =>
        trailheads.map((loc, _) => trailheadScore(parsed, loc).size).sum.toString()
      case Part.Two =>
        trailheads.map((loc, _) => trailheadRating(parsed, loc, List.empty)).sum.toString()
    
}
