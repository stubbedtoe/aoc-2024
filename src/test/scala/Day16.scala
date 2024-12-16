import Aoc2024.{aoc2024, Part, Input}
import Aoc2024.Day16.{part1}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day16 extends munit.FunSuite {
  test("Day16 - part 1") {
    val expected = "7036"
    val obtained = aoc2024(16, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day16 - part 1 - second example") {
    val input = """#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################""".split("\\n").toList
    val expected = "11048"
    val obtained = part1(input)
    assertEquals(expected, obtained)
  }

  test("Day16 - part 2") {
    val expected = ""
    val obtained = aoc2024(16, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
