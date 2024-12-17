import Aoc2024.{aoc2024, Part, Input}
import Aoc2024.Day17.{reader, Register, Machine }

val empty = Map(Register.A -> 0L, Register.B -> 0L, Register.C -> 0L)

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day17 extends munit.FunSuite {
  test("Day17 - part 1") {
    val expected = "4,6,3,5,6,3,5,2,1,0"
    val obtained = aoc2024(17, Part.One, Input.Test)
    assertEquals(expected, obtained)
  }

  test("Day17 - example 1") {
    val registers = empty.updated(Register.C, 9L)
    val machine = Machine(registers = registers, pointer = 0, output = "")
    val instructions = Array(2,6)
    val obtained = reader(instructions, machine)
    assertEquals(1L, obtained.registers(Register.B))
  }

  test("Day17 - example 2") {
    val registers = empty.updated(Register.A, 10L)
    val machine = Machine(registers = registers, pointer = 0, output = "")
    val instructions = Array(5,0,5,1,5,4)
    val obtained = reader(instructions, machine)
    assertEquals("0,1,2", obtained.output)
  }

  test("Day17 - example 3") {
    val registers = empty.updated(Register.A, 2024L)
    val machine = Machine(registers = registers, pointer = 0, output = "")
    val instructions = Array(0,1,5,4,3,0)
    val obtained = reader(instructions, machine)
    assertEquals("4,2,5,6,7,7,7,7,3,1,0", obtained.output)
    assertEquals(0L, obtained.registers(Register.A))
  }

  test("Day17 - example 4") {
    val registers = empty.updated(Register.B, 29L)
    val machine = Machine(registers = registers, pointer = 0, output = "")
    val instructions = Array(1,7)
    val obtained = reader(instructions, machine)
    assertEquals(26L, obtained.registers(Register.B))
  }

  test("Day17 - example 5") {
    val registers = empty.updated(Register.B, 2024L).updated(Register.C, 43690L)
    val machine = Machine(registers = registers, pointer = 0, output = "")
    val instructions = Array(4,0)
    val obtained = reader(instructions, machine)
    assertEquals(44354L, obtained.registers(Register.B))
  }

  test("Day17 - part 2") {
    val expected = ""
    val obtained = aoc2024(17, Part.Two, Input.Test)
    assertEquals(expected, obtained)
  }
}
