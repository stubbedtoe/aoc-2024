package Aoc2024

import scala.annotation.tailrec

object Day17 {
  enum Register:
    case A, B, C
  type Registers = Map[Register, Long]

  case class Machine(registers: Registers, pointer: Int, output: String):
    def interpretCombo(combo: Int): Long =
      combo match
        case 4 => registers(Register.A)
        case 5 => registers(Register.B)
        case 6 => registers(Register.C)
        case _ => combo.toLong

    def adv(combo: Int): Machine =
      // division
      val numerator = registers(Register.A)
      val denominator = Math.pow(2, this.interpretCombo(combo))
      this.copy(
        registers = registers.updated(Register.A, Math.floor(numerator / denominator).toLong),
        pointer = pointer + 2,
      )
      
    def bxl(literal: Int): Machine =
      // bitwise XOR
      val result = registers(Register.B) ^ literal
      this.copy(
        registers = registers.updated(Register.B, result),
        pointer = pointer + 2,
      )
      

    def bst(combo: Int): Machine =
      this.copy(
        registers = registers.updated(Register.B, this.interpretCombo(combo) % 8),
        pointer = pointer + 2,
      )
      
    def jnz(literal: Int): Machine =
      if (registers(Register.A) == 0)
        this.copy(pointer = pointer + 2)
      else
        this.copy(pointer = literal)
    
    def bxc(ignored: Int): Machine =
      val result = registers(Register.B) ^ registers(Register.C)
      this.copy(
        registers = registers.updated(Register.B, result),
        pointer = pointer + 2,
      )

    def out(combo: Int): Machine =
      val result = this.interpretCombo(combo) % 8
      this.copy(
        output = if output.length > 0 then output ++ s",$result" else result.toString(),
        pointer = pointer + 2,
      )
    
    def bdv(combo: Int): Machine =
      val numerator = registers(Register.A)
      val denominator = Math.pow(2, this.interpretCombo(combo))
      this.copy(
        registers = registers.updated(Register.B, (numerator / denominator).toLong),
        pointer = pointer + 2,
      )

    def cdv(combo: Int): Machine =
      val numerator = registers(Register.A)
      val denominator = Math.pow(2, this.interpretCombo(combo))
      this.copy(
        registers = registers.updated(Register.C, (numerator / denominator).toLong),
        pointer = pointer + 2,
      )
      
  end Machine


  @tailrec
  def reader(instructions: Array[Int], machine: Machine): Machine =
    if machine.pointer < (instructions.length - 1) then
      val opcode = instructions(machine.pointer)
      val arg = instructions(machine.pointer + 1)
      val function =
        opcode match
          case 0 => machine.adv
          case 1 => machine.bxl
          case 2 => machine.bst
          case 3 => machine.jnz
          case 4 => machine.bxc
          case 5 => machine.out
          case 6 => machine.bdv
          case 7 => machine.cdv

      reader(instructions, function(arg))
    else
      machine

  def parse(lines: List[String]): (Machine, Array[Int]) =
    if lines.head == "Register A: 729" then
      // test
      val registers = Map(
        Register.A -> 729L,
        Register.B -> 0L,
        Register.C -> 0L,
      )
      val instructions = Array(0, 1, 5, 4, 3, 0)
      (Machine(registers = registers, pointer = 0, output = ""), instructions)
    else
      // actual
      val registers = Map(
        Register.A -> 66752888L,
        Register.B -> 0L,
        Register.C -> 0L,
      )
      val instructions = Array(2,4,1,7,7,5,1,7,0,3,4,1,5,5,3,0)
      (Machine(registers = registers, pointer = 0, output = ""), instructions)
      

  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  def part1(lines: List[String]): String =
    val (machine, instructions) = parse(lines)
    val afterInitialRun = reader(instructions, machine)
    afterInitialRun.output

  def part2(lines: List[String]): String =
    "Haven't solved part 2 yet"
}
