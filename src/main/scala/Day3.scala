import scala.util.matching.Regex
import scala.annotation.tailrec

object Day3 {
  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def totalForLine(line: String): Int =
    val regex = new Regex("mul\\(([0-9]+),([0-9]+)\\)", "x", "y")
    val matches = regex.findAllMatchIn(line).toList
    matches.map(m => m.group("x").toInt * m.group("y").toInt).sum

  def part1(lines: List[String]): String =
    totalForLine(lines.flatten.mkString).toString()

  @tailrec  
  def ignoreSubstrings(saved: String, input: String): String =
    input match
      case "" => saved
      case ignore if ignore.startsWith("don't()") =>
        ignoreSubstrings(saved, input.drop(7).dropWhile(c => c != 'd'))
      case save if save.startsWith("do()") =>
        val addThis = save.drop(4).takeWhile(c => c != 'd')
        ignoreSubstrings(saved.concat(addThis), input.drop(addThis.length()))
      case _ =>
        ignoreSubstrings(saved.concat(input.take(1)), input.drop(1))

  def part2(lines: List[String]): String =
    // val example = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    // totalForLine(ignoreSubstrings("", example)).toString()
    totalForLine(ignoreSubstrings("", lines.flatten.mkString)).toString()
}
