// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day4 extends munit.FunSuite {
  test("Day4 - part 1") {
    val expected = "18"
    val obtained = aoc2024(4, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day4 - part 2") {
    val expected = "9"
    val obtained = aoc2024(4, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
