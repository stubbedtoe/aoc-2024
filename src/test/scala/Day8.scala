import Aoc2024.{aoc2024, Part, Input}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day8 extends munit.FunSuite {
  test("Day8 - part 1") {
    val expected = "14"
    val obtained = aoc2024(8, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day8 - part 2") {
    val expected = "34"
    val obtained = aoc2024(8, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
