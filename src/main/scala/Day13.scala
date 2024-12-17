package Aoc2024

import scala.util.matching.Regex
import scala.annotation.tailrec

object Day13 {
  @tailrec
  def highestCommonFactor(num1: Long, num2: Long): Long =
    if num2 == 0 then
      num1
    else
      highestCommonFactor(num2, num1 % num2)


  case class Button(x: Int, y: Int, cost: Int)
  case class Claw(buttonA: Button, buttonB: Button, prize: (Long, Long)):
    def processForPart2(): Claw =
      val toAdd = 10000000000000L
      this.copy(prize = (prize._1 + toAdd, prize._2 + toAdd))

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

    // def isSolvablePart2(): Long =
    //   lazy val solveCosts: LazyList[Long] = 
    //     for {
    //       timesA <- (0L to 1000000000000L)
    //       timesB <- (1000000000000L to 0L)
    //       if ((timesA * buttonA.x + timesB * buttonB.x) == prize._1 && (timesA * buttonA.y + timesB * buttonB.y) == prize._2)
    //     } yield (timesA * buttonA.cost) + (timesB * buttonB.cost)
    //   solveCosts.headOption match
    //     case Some(n) =>
    //       n
    //     case None =>
    //       0

    @tailrec
    final def findMinimumCost(current: (Long, Long), times: Map[Button, Long]): Long =
      if (current == prize) then
        (times(buttonA) * buttonA.cost) + (times(buttonB) * buttonB.cost)
      else if (current._1 > prize._1 || current._2 > prize._2)
         // remove a press of buttom B
        // println(s"un-pressing button B at ${current}")
        findMinimumCost(
          current = (current._1 - buttonB.x, current._2 - buttonB.y),
          times = times.updated(buttonB, times(buttonB) - 1)
        )
      else if (times(buttonB) == 0) // it's not possible
        println(s"not possible for $this")
        0
      else
        findMinimumCost(
          current = (current._1 + buttonA.x, current._2 + buttonA.y),
          times = times.updated(buttonA, times(buttonA) + 1)
        )



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
    val claws = splitIntoClaws(lines).map(_.processForPart2())
    // claws.map(_.isSolvablePart2()).sum.toString()
    claws.map(claw =>
      val x = Math.floor(claw.prize._1 / claw.buttonB.x).toLong
      val y = Math.floor(claw.prize._2 / claw.buttonB.y).toLong
      val timesB = Math.min(x, y)
      println(s"pressing B initially $timesB times")
      println(s"current: ${(claw.buttonB.x * timesB, claw.buttonB.y * timesB)}")
      claw.findMinimumCost(current = (claw.buttonB.x * timesB, claw.buttonB.y * timesB), times = Map(claw.buttonA -> 0, claw.buttonB -> timesB))  
    ).sum.toString()
}
