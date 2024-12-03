// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day2 extends munit.FunSuite {
  test("Day2 - part 1") {
    val expected = "2"
    val obtained = aoc2024(2, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day2 - part 2") {
    val expected = "4"
    val obtained = aoc2024(2, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
