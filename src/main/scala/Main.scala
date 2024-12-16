package Aoc2024

import scala.util.CommandLineParser
import Day1.{ solve => day1 }
import Day2.{ solve => day2 }
import Day3.{ solve => day3 }
import Day4.{ solve => day4 }
import Day5.{ solve => day5 }
import Day6.{ solve => day6 }
import Day7.{ solve => day7 }
import Day8.{ solve => day8 }
import Day9.{ solve => day9 }
import Day10.{ solve => day10 }
import Day11.{ solve => day11 }
import Day12.{ solve => day12 }
import Day14.{ solve => day14 }
import Day15.{ solve => day15 }
import Day16.{ solve => day16 }
import Day13.{ solve => day13 }
// new days added here 

enum Input:
  case Test, Actual

enum Part:
  case One, Two

given CommandLineParser.FromString[Input] with
  def fromString(value: String): Input = Input.valueOf(value)

given CommandLineParser.FromString[Part] with
  def fromString(value: String): Part = Part.valueOf(value)

@main def aoc2024(day: Int, part: Part = Part.One, input: Input = Input.Test) =
  val filePath = s"src/inputs/${input}/day${day}.txt"
  val notDoneYet = "Error! I haven't done this day yet"
  val completedDays: Map[Int, (List[String], Part) => String] = Map(
    1 -> day1,
    2 -> day2,
    3 -> day3,
    4 -> day4,
    5 -> day5,
    6 -> day6,
    7 -> day7,
    8 -> day8,
    9 -> day9,
    10 -> day10,
    11 -> day11,
    12 -> day12,
    14 -> day14,
    15 -> day15,
    16 -> day16,
    13 -> day13,
    // mappings added here
  )
  var result = completedDays.get(day) match {
    case Some(function) =>
      try {
        val lines = io.Source.fromFile(filePath).getLines().toList
        function(lines, part)
      } catch {
        case e => println(e) // notDoneYet
      }
      
    case None => notDoneYet
  }
  
  println()
  println(f"-----Day ${day}; Part ${part}; ${input} Input--------")
  println(result)
  println("--------------")
  println()
  result
