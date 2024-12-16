import Aoc2024.{aoc2024, Part, Input}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day14 extends munit.FunSuite {
  test("Day14 - part 1") {
    val expected = "12"
    val obtained = aoc2024(14, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day14 - part 2".ignore) {
    val expected = ""
    val obtained = aoc2024(14, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
