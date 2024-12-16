import Aoc2024.{aoc2024, Part, Input}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day15 extends munit.FunSuite {
  test("Day15 - part 1") {
    val expected = "10092"
    val obtained = aoc2024(15, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day15 - part 2") {
    val expected = "9021"
    val obtained = aoc2024(15, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
