import scala.util.CommandLineParser
import Day1.{ solve => day1 }
import Day2.{ solve => day2 }
import Day3.{ solve => day3 }
import Day4.{ solve => day4 }
import Day5.{ solve => day5 }
import Day6.{ solve => day6 }
import Day7.{ solve => day7 }
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
