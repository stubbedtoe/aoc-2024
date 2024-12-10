import scala.annotation.tailrec
sealed trait Fragment

case class Empty() extends Fragment
case class File(id: Long) extends Fragment

object Day9 {
  def printFragment(fragment: Fragment): String =
    fragment match
      case Empty() => "."
      case File(id) => if id > 9 then s"($id)" else id.toString()

  def printBlocks(blocks: Array[Fragment]): String =
    blocks.map(printFragment).flatten.mkString

  def solve(lines: List[String], part: Part): String =
    part match {
        case Part.One => part1(lines)
        case Part.Two => part2(lines)
    }

  @tailrec
  def moveBlocksPt1(blocks: Array[Fragment], target: Long): Array[Fragment] =
    blocks.indexOf(Empty()) match
      case -1 => throw new Error("no first empty block found")
      case firstEmpty => 
        if (firstEmpty == target)
          blocks
        else
          blocks.findLast(_.isInstanceOf[File]) match
            case None => throw new Error("no last file block found")
            case Some(file) =>
              blocks.lastIndexOf(file) match
                case -1 => throw new Error("no last file block found")
                case lastFileIndex =>
                  blocks.update(firstEmpty, file)
                  blocks.update(lastFileIndex, Empty())
                  moveBlocksPt1(blocks, target)

  @tailrec
  def moveBlocksPt2(blocks: Array[Fragment], target: Long): Array[Fragment] =
    if (target == 0)
      blocks
    else
      val indicesOfBlocksToMove =
        blocks
          .zipWithIndex
          .filter((block, _) =>
            block match
              case Empty() => false
              case File(id) => id == target
          )
          .map(_._2)

      val numberOfBlocksToMove = indicesOfBlocksToMove.length

      if (numberOfBlocksToMove == 0)
        // 0 blocks to move for target
        moveBlocksPt2(blocks, target - 1)
      else
        val maybeFirstEmpty =
          Range(0, indicesOfBlocksToMove.head + 1)
            .find(i =>
              Range(i, i + numberOfBlocksToMove).forall(n => blocks(n).isInstanceOf[Empty])
            )

        maybeFirstEmpty match
          case None =>
            // could not find enough contiguous empty blocks
            moveBlocksPt2(blocks, target - 1)
          case Some((firstSpaceAvailable)) =>
            Range(0, numberOfBlocksToMove).foreach(i =>
              blocks.update(firstSpaceAvailable + i, File(target))
              blocks.update(indicesOfBlocksToMove(i), Empty())
            )
            moveBlocksPt2(blocks, target - 1)

  def parse(line: String): Array[Fragment] =
    line
      .toCharArray()
      .zipWithIndex
      .map((c, i) =>
        if (i % 2 == 0)
          val id = i / 2
          List.fill(c.asDigit)(File(i / 2))
        else
          List.fill(c.asDigit)(Empty())
      ).flatten
      .toArray

  def totalFileBlocks(blocks: Array[Fragment]) =
    blocks.filter(b => b match
      case File(_) => true
      case Empty() => false
    ).length

  def calcChecksum(blocks: Array[Fragment]) =
    blocks.zipWithIndex.map((f, i) =>
      f match
        case File(id) => i * id
        case Empty() => 0
    ).sum

  def part1(lines: List[String]): String =
    val parsed = parse(lines.head)
    val moved = moveBlocksPt1(parsed, totalFileBlocks(parsed))
    calcChecksum(moved).toString()

  def part2(lines: List[String]): String =
    val parsed = parse(lines.head)
    val lastFile = parsed.findLast(_.isInstanceOf[File])
    lastFile match
      case Some(File(id)) =>
        val moved = moveBlocksPt2(parsed, id)
        calcChecksum(moved).toString()
      case _ =>
        "no file ids found"
}
