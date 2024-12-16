package Aoc2024

import scala.util.matching.Regex

object Day14 {
  type Location = (Int, Int)

  class Robot(val position: Location, val velocity: Location):
    
    def move(maxX: Int, maxY: Int): Robot =
      var nextX = position._1 + velocity._1
      if (nextX < 0)
        nextX = maxX + nextX
      else if (nextX >= maxX)
        nextX = nextX % maxX
      var nextY = position._2 + velocity._2
      if (nextY < 0)
        nextY = maxY + nextY
      else if (nextY >= maxY)
        nextY = nextY % maxY
      Robot(position = (nextX, nextY), velocity = velocity)

    def movetimes(times: Int, maxX: Int, maxY: Int): Robot =
      (1 to times).foldLeft(this)((robot, i) =>
        robot.move(maxX, maxY)
      )
    

    override def toString: String = s"Robot location: $position and velocity: $velocity"

  case class SetUp(robots: List[Robot], width: Int, height: Int)


  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def parseRobot(line: String): Robot =
    val regex = new Regex("p=([0-9]+),([0-9]+) v=(\\-?[0-9]+),(\\-?[0-9]+)", "x", "y", "vx", "vy")
    val _match = regex.findAllMatchIn(line).toList.head
    val position = (_match.group("x").toInt, _match.group("y").toInt)
    val velocity = (_match.group("vx").toInt, _match.group("vy").toInt)
    Robot(position = position, velocity = velocity )

  def parse(lines: List[String]): SetUp =
    val regex = new Regex("([0-9]+),([0-9]+)", "width", "height")
    val _match = regex.findAllMatchIn(lines.head).toList.head
    val width = _match.group("width").toInt
    val height = _match.group("height").toInt
    
    val robots = lines.tail.map(parseRobot)
    SetUp(robots = robots, width = width , height = height)

  def part1(lines: List[String]): String =
    val setup = parse(lines)
    val (robots, width, height) = (setup.robots, setup.width, setup.height)
  
    val movedRobots = robots.map(_.movetimes(100, width, height))
    val ignoreRow = Math.floor(height / 2)
    val ignoreColumn = Math.floor(width / 2)

    val topLeft = movedRobots.count(r =>
      r.position._1 < ignoreColumn && r.position._2 < ignoreRow
    )
    val topRight = movedRobots.count(r =>
      r.position._1 > ignoreColumn && r.position._2 < ignoreRow
    )
    val bottomLeft = movedRobots.count(r =>
      r.position._1 < ignoreColumn && r.position._2 > ignoreRow
    )
    val bottomRight = movedRobots.count(r =>
      r.position._1 > ignoreColumn && r.position._2 > ignoreRow
    )

    (topLeft * topRight * bottomLeft * bottomRight).toString()


  def part2(lines: List[String]): String =
    val setup = parse(lines)
    val (width, height) = (setup.width, setup.height)
    var secondsElapsed = 6071
    var movedRobots = setup.robots.map(_.movetimes(secondsElapsed, width, height))
    println("next: (move or stop)")
    var input = scala.io.StdIn.readLine()
    while(input != "stop") {
      secondsElapsed = secondsElapsed + 101
      println(s"after $secondsElapsed seconds:")
      movedRobots = movedRobots.map(_.movetimes(101, width, height))
      Range(0, height).map(y =>
        println(Range(0, width).map(x =>
          val n = movedRobots.count(r =>
            r.position._1 == x && r.position._2 == y
          )
          if (n == 0) then " . " else s" # "
        ).reduce(_ ++ _))   
      )
      println("next: (move or stop)")
      input = scala.io.StdIn.readLine()
    }
    
    ""
}
