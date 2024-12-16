import Aoc2024.{aoc2024, Part, Input}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day5 extends munit.FunSuite {
  test("Day5 - part 1") {
    val expected = "143"
    val obtained = aoc2024(5, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day5 - part 2") {
    val expected = "123"
    val obtained = aoc2024(5, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
