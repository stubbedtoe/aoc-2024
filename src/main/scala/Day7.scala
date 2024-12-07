import scala.util.matching.Regex

enum Operator:
  case Multiply, Add

case class Line(target: Long, factors: List[Long])

def canBeCalculated(target: Long, current: Long, others: List[Long], part: Part): Boolean =
  others match
    case Nil => current == target
    case x::xs =>
      if (current > target)
        false
      else
        val part1 = canBeCalculated(target, current * x, xs, part) || canBeCalculated(target, current + x, xs, part)
        part match
          case Part.One => part1
          case Part.Two => part1 || canBeCalculated(target, s"$current$x".toLong, xs, part)

def parseLine(line: String): Line =
  val ruleRegex = new Regex("([0-9]+):(( [0-9]+)+)", "target", "options")
  val matched = ruleRegex.findAllMatchIn(line).toList.head
  val target = matched.group("target").toLong
  val splitFactors = matched.group("options").split(" ").tail.map(_.toLong)
  Line(target, splitFactors.toList)

object Day7 {
  def solve(lines: List[String], part: Part): String =
    lines
      .map(parseLine)
      .filter(l => canBeCalculated(l.target, l.factors.head, l.factors.tail, part))
      .map(_.target)
      .sum()
      .toString()
}
