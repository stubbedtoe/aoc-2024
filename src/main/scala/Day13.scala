package Aoc2024

import scala.util.matching.Regex

object Day13 {
  case class Button(x: Int, y: Int, cost: Int)
  case class Claw(buttonA: Button, buttonB: Button, prize: (Long, Long)):
    def isSolvable(): Int =
      val solveCosts = 
        for {
          timesA <- (0 to 100)
          timesB <- (0 to 100)
          if ((timesA * buttonA.x + timesB * buttonB.x) == prize._1 && (timesA * buttonA.y + timesB * buttonB.y) == prize._2)
        } yield (timesA * buttonA.cost) + (timesB * buttonB.cost)
      solveCosts.minOption match
        case Some(n) =>
          n
        case None =>
          0

  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def splitIntoClaws(lines: List[String]): List[Claw] = 
    val clawsGroups = lines.grouped(4).map(_.reduce(_ ++ _)).toList
    val regex = new Regex(
      "Button A: X\\+([0-9]+), Y\\+([0-9]+)Button B: X\\+([0-9]+), Y\\+([0-9]+)Prize: X=([0-9]+), Y=([0-9]+)",
      "buttonA_X", "buttonA_Y",
      "buttonB_X", "buttonB_Y",
      "prizeX", "prizeY"
    )
    clawsGroups.map(s =>
      val matched = regex.findAllMatchIn(s.strip()).toList
      val buttonA = Button(x = matched.head.group("buttonA_X").toInt, y = matched.head.group("buttonA_Y").toInt, cost = 3)
      val buttonB = Button(x = matched.head.group("buttonB_X").toInt, y = matched.head.group("buttonB_Y").toInt, cost = 1)
      Claw(buttonA = buttonA, buttonB = buttonB, prize = (matched.head.group("prizeX").toLong, matched.head.group("prizeY").toLong))
    )

  def part1(lines: List[String]): String =
    val claws = splitIntoClaws(lines)
    claws.map(_.isSolvable()).sum.toString()

  def part2(lines: List[String]): String =
    "Haven't solved part 2 yet"
}
