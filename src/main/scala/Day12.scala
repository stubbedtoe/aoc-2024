import scala.annotation.tailrec
import scala.collection.immutable.NumericRange.Inclusive


object Day12 {
  type Location = (Int, Int)
  enum Fence:
    case Vertical, Horizontal
  case class FenceSegment(static: Int, from: Int, to: Int, orientation: Fence)
  case class AllBlocks(blocks: Map[Location, Char], width: Int, height: Int)

  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def parse(lines: List[String]): AllBlocks =
    val blocks = 
      Map.from(lines
        .zipWithIndex
        .map((line, y) =>
          line
            .toCharArray()
            .zipWithIndex
            .map((c, x) =>
              (x, y) -> c  
            )
        ).flatten)
    AllBlocks(blocks = blocks, width = lines.head.length, height = lines.length)

  def getNeighbours(location: Location): Set[Location] =
    val (x, y) = location
    
    (for {
      diffX <- (-1 to 1)
      diffY <- (-1 to 1)
      if List(diffX, diffY).contains(0) && (x+diffX, y+diffY) != location
    } yield (x+diffX, y+diffY)).toSet

  def getPerimeter(plot: Set[Location]): Int =
    plot.toList.map(location => 
      val neighbours = getNeighbours(location)
      neighbours.diff(plot).size
    ).sum

  def getSegments(plot: Set[Location]): List[(Location, Fence)] =
    plot.toList.foldLeft(List.empty)((acc, location) =>
      val (x, y) = location
      val above = ((x, y - 1), ((x, y), Fence.Horizontal))
      val below = ((x, y + 1), ((x, y + 1), Fence.Horizontal))
      val left = ((x - 1, y), ((x, y), Fence.Vertical))
      val right = ((x + 1, y), ((x + 1, y), Fence.Vertical))
      val segments: List[(Location, Fence)] = List(above, below, left, right)
        .filterNot(t => plot.contains(t._1)
        ).map(t => t._2)
      segments ++ acc
    )

  def removeSegments(segments: List[(Location, Fence)]): List[FenceSegment] =
    // println("")
    val segs: List[FenceSegment] = segments.foldLeft(List.empty)((acc, segment) =>
      val ((x, y), orientation) = segment
      val (static, dynamic) = if orientation == Fence.Vertical then (x, y) else (y, x)
      acc.find(seg => seg.static == static && seg.orientation == orientation) match
        case Some(seg) =>
          val others = acc.filterNot(_ == seg)
          if ((seg.from - 1 to seg.to).contains(dynamic))
            seg.copy(from = seg.from - 1)::others
          else if ((seg.from to seg.to + 1).contains(dynamic))
            seg.copy(to = seg.to + 1)::others
          else
            FenceSegment(static = static, from = dynamic, to = dynamic, orientation = orientation)::acc
        case None =>
          FenceSegment(static = static, from = dynamic, to = dynamic, orientation = orientation)::acc
    )
    // segs.map(println)
    segs


  @tailrec 
  def separateSets(set: (Set[Location], Char), list: List[(Set[Location], Char)]): List[(Set[Location], Char)] =
    set._1.toList match
      case location::rest =>
        val neighbours = getNeighbours(location)
        list.find(s => s._1.intersect(neighbours).size > 0) match
          case Some(l) =>
            val withLocation = (l._1.incl(location), l._2)
            val others = list.filterNot(_ == l)
            separateSets((rest.toSet, set._2), withLocation::others)
          case None =>
            separateSets((rest.toSet, set._2), (Set(location), set._2)::list)
      case _ =>
        list

  @tailrec
  def combineNeighbouringSets(processed: List[(Set[Location], Char)], toProcess: List[(Set[Location], Char)]): List[(Set[Location], Char)] =
    toProcess match
      case (set, c)::rest =>
        processed.find((l, c2) => l.exists(location => set.intersect(getNeighbours(location)).size > 0 && c == c2)) match
          case Some(l) =>
            val withSet = (l._1.union(set), c)
            val others = processed.filterNot(_ == l)
            // println(s"$l and $set are neighbours and they both grow $c")
            combineNeighbouringSets(withSet::others, rest)
          case None =>
            // println(s"cound not find a neighbouring set growing $c for $set")
            combineNeighbouringSets((set, c)::processed, rest)
      case Nil => 
        processed

  
  @tailrec
  def keepCombining(previous: List[(Set[Location], Char)], current: List[(Set[Location], Char)]): List[(Set[Location], Char)] =
    if (previous == current)
      current
    else
      keepCombining(current, combineNeighbouringSets(List.empty, current).sorted)


  def part1(lines: List[String]): String =
    val parsed = parse(lines)
    val grouped = parsed.blocks.groupBy(_._2).map((c, m) => (m.keySet, c)).toList
    
    val separated = grouped.map(s => separateSets(s, List.empty)).flatten
   
    val combined = keepCombining(List.empty, separated.sorted)
    combined.map(_._1).map(s => s.size * getPerimeter(s)).sum.toString


  def part2(lines: List[String]): String =
    val parsed = parse(lines)
    val grouped = parsed.blocks.groupBy(_._2).map((c, m) => (m.keySet, c)).toList
    
    val separated = grouped.map(s => separateSets(s, List.empty)).flatten
   
    val combined = keepCombining(List.empty, separated.sorted)
    combined.map(_._1).map(s => s.size * removeSegments(getSegments(s)).size).sum.toString()
    
}
