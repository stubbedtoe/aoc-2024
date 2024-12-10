// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day9 extends munit.FunSuite {
  test("Day9 - part 1") {
    val expected = "1928"
    val obtained = aoc2024(9, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day9 - part 2") {
    val expected = "2858"
    val obtained = aoc2024(9, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
