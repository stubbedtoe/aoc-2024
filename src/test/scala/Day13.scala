import Aoc2024.{aoc2024, Part, Input}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day13 extends munit.FunSuite {
  test("Day13 - part 1") {
    val expected = "480"
    val obtained = aoc2024(13, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day13 - part 2") {
    val expected = ""
    val obtained = aoc2024(13, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
