import scala.util.matching.Regex
import scala.annotation.tailrec

object Day5 {
  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  type Rule = (Int, Int)
  type Update = Array[Int]

  def updateIsValid(update: Update, rules: List[Rule]): Boolean =
    update.zipWithIndex.forall((n, i) =>
      rules.forall((before, after) =>
        if (before == n)
          val otherInUpdate = update.indexOf(after)
          otherInUpdate == -1 || otherInUpdate > i
        else if (after == n)
          val otherInUpdate = update.indexOf(before)
          otherInUpdate == -1 || otherInUpdate < i
        else
          // the rule does not apply
          true
      )
    )

  def findMiddleValue(update: Update): Int =
    val middle = Math.floor(update.length / 2).toInt
    update(middle)

  @tailrec
  def reorderPages (sorted: List[Int], toSort: List[Int], rules: List[Rule]): Update =
    toSort match
      case Nil => sorted.toArray
      case x::Nil => (sorted ++ List(x)).toArray
      case x::tail =>
        rules.find((before, after) => x == after && tail.contains(before)) match
          case None =>
            // x is in the right place
            reorderPages(sorted ++ List(x), tail, rules)
          case Some(_) =>
            // moving x to the end of tail
            reorderPages(sorted, tail ++ List(x), rules)
    

  def parse(lines: List[String]):(List[Rule], List[Update]) =
    val firstPart = lines.takeWhile(line => line != "")
    val secondPart = lines.dropWhile(line => line != "").tail
    val ruleRegex = new Regex("([0-9]+)\\|([0-9]+)", "x", "y")

    val rules = firstPart.map(line =>
      val rule = ruleRegex.findAllMatchIn(line).toList.head
      (rule.group("x").toInt, rule.group("y").toInt)
    )

    val updates = secondPart.map(line =>
      line.split(",").map(_.toInt)
    )

    (rules, updates)

  def part1(lines: List[String]): String =
    val (rules, updates) = parse(lines)
    val validUpdates = updates
      .filter(update => updateIsValid(update, rules))

    val middleValues = validUpdates.map(findMiddleValue)
    
    middleValues.sum.toString()

  def part2(lines: List[String]): String =
    val (rules, updates) = parse(lines)
    val invalidUpdates = updates
      .filter(update => !updateIsValid(update, rules))

    val sortedUpdates = invalidUpdates.map(update => reorderPages(List.empty, update.toList, rules))

    val middleValues = sortedUpdates.map(findMiddleValue)
    
    middleValues.sum.toString()
}
