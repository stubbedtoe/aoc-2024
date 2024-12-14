// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day12 extends munit.FunSuite {
  test("Day12 - part 1") {
    val expected = "1930"
    val obtained = aoc2024(12, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day12 - part 2") {
    val expected = "1206"
    val obtained = aoc2024(12, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
