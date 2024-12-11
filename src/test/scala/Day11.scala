// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day11 extends munit.FunSuite {
  test("Day11 - part 1") {
    val expected = "55312"
    val obtained = aoc2024(11, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day11 - part 2".ignore) {
    // not given a test answer for this part
    val expected = ""
    val obtained = aoc2024(11, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
