import Aoc2024.{aoc2024, Part, Input}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day1 extends munit.FunSuite {
  test("day1 - part 1") {
    val expected = "11"
    val obtained = aoc2024(1, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("day1 - part 2") {
    val expected = "31"
    val obtained = aoc2024(1, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
