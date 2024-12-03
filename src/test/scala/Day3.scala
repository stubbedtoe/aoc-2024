// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day3 extends munit.FunSuite {
  test("Day3 - part 1") {
    val expected = "161"
    val obtained = aoc2024(3, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day3 - part 2") {
    val expected = "161"
    val obtained = aoc2024(3, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
