object Day11 {
  def memoize[I, O](f: I => O): I => O = new collection.mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  lazy val blink: ((Long, Int, Int)) => Long = memoize {
    case (stone, iteration, target) =>
      if (iteration == target)
        1
      else
        val s = stone.toString()
        stone match
          case 0 =>
            blink((1, iteration + 1, target))
          case n if s.length % 2 == 0 =>
            val l = s.length / 2
            val s1 = blink((s.take(l).toLong, iteration + 1, target))
            val s2 = blink((s.drop(l).toLong, iteration + 1, target))
            s1 + s2
          case _ =>
            blink((stone * 2024L, iteration + 1, target))
  }

  def solve(lines: List[String], part: Part): String =
    val stones = lines.head.split(" ").map(_.toLong).toList
    val iterations = 
      part match {
          case Part.One => 25
          case Part.Two => 75
      }
    stones
      .map(s => blink((s, 0, iterations)))
      .sum
      .toString()
}
