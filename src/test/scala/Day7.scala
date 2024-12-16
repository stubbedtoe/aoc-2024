import Aoc2024.{aoc2024, Part, Input}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day7 extends munit.FunSuite {
  test("Day7 - part 1") {
    val expected = "3749"
    val obtained = aoc2024(7, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day7 - part 2") {
    val expected = "11387"
    val obtained = aoc2024(7, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
