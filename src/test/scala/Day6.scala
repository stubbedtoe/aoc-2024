import Aoc2024.{aoc2024, Part, Input}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day6 extends munit.FunSuite {
  test("Day6 - part 1") {
    val expected = "41"
    val obtained = aoc2024(6, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day6 - part 2") {
    val expected = "6"
    val obtained = aoc2024(6, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
