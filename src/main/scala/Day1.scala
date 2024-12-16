package Aoc2024

import scala.collection.View.Zip

object Day1 {
  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def parse(lines: List[String]): List[(Int, Int)] =
    lines.map(line =>
        line.split("   ").toList match {
            case first::second::List() => (first.toInt, second.toInt)
            case _ => (0,0)
        })

  def part1(lines: List[String]): String =
    val numbers = parse(lines)

    val firstList = numbers.map((first, _) => first).sorted
    val secondList = numbers.map((_, second) => second).sorted

    val difference = new Zip(firstList, secondList).toList.map((first, second) => Math.abs(first - second)).sum
    difference.toString()

  def part2(lines: List[String]): String =
    val numbers = parse(lines)

    val firstList = numbers.map((first, _) => first)
    val secondList = numbers.map((_, second) => second)

    val frequencies = firstList.map(n => secondList.count(_ == n) * n).sum
    frequencies.toString()
}
