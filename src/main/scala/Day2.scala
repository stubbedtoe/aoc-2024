import scala.annotation.tailrec

enum Direction:
  case Increasing, Decreasing, None


object Day2 {
  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def parse(lines: List[String]): List[List[Int]] =
    lines.map(_.split(" ").map(_.toInt).toList)

  def reportIncreasing(report: List[Int]): Boolean =
    report.sorted == report

  def reportDecreasing(report: List[Int]): Boolean =
    report.sorted.reverse == report

  def reportSmallSteps(report: List[Int]): Boolean =
    report.sliding(2, 1).toList.forall(pair =>
      pair match
        case a::b::Nil =>
          val diff = Math.abs(a - b)
          diff <= 3 && diff >= 1
        case _ =>
          false
    )

  def isSafe(report: List[Int]): Boolean =
    (reportDecreasing(report) || reportIncreasing(report)) && reportSmallSteps(report)

  
  def part1(lines: List[String]): String =
    parse(lines).count(isSafe).toString()


  def isSafeWithOneLevelRemoved(report: List[Int]): Boolean =
    if (isSafe(report))
      true
    else

      val withOneRemoved = for {
        i <- Range(0, report.length)
      } yield report.take(i) ++ report.drop(1 + i)

      withOneRemoved.toList.exists(isSafe)

    

  def part2(lines: List[String]): String =  
    parse(lines).count(isSafeWithOneLevelRemoved).toString()
}
