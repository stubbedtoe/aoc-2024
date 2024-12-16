import Aoc2024.{aoc2024, Part, Input}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day10 extends munit.FunSuite {
  test("Day10 - part 1") {
    val expected = "36"
    val obtained = aoc2024(10, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day10 - part 2") {
    val expected = "81"
    val obtained = aoc2024(10, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
