import scala.annotation.tailrec
sealed trait Fragment

case class Empty() extends Fragment
case class File(id: Long) extends Fragment

// pt2
case class Block(id: Long, size: Int, freeSpaceFollowing: Long)

object Day9 {
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
  def moveBlocksPt2(blocks: List[Block], blockToMove: Block): List[Block] =
    if (blockToMove.id == 0)
      blocks
    else
      // only edit the last element of this:
      // val leftList = blocks.takeWhile(b => (b.id != blockToMove.id - 1) && (b.freeSpaceFollowing < blockToMove.size))


      val blocksWithoutOneToMove = blocks.filter(b => b.id != blockToMove.id)
      val nextBlock = blocks.find(b => b.id == (blockToMove.id - 1))
      val leftList = blocksWithoutOneToMove.takeWhile(b => b.freeSpaceFollowing < blockToMove.size)
      val leftListIncludesNextBlock = leftList.exists(b => b.id == blockToMove.id - 1)
      
      if (leftListIncludesNextBlock || leftList.length == blocks.length - 1)
        // no space, move on
        nextBlock match
          case Some(block) =>
            // println(s"no room for ${blockToMove.id}, trying ${block.id}")
            moveBlocksPt2(blocks, block)
          case None =>
            // println(s"could not find block with id ${blockToMove.id - 1}")
            blocks
      else
        blocksWithoutOneToMove.drop(leftList.length) match
          case head::rest =>
             nextBlock match
              case Some(block) =>
                // println(s"moving ${blockToMove.id} beside ${head.id}. Now trying ${block.id}")
                // update the rest
                val preceedingBlock = blocks
                  .sliding(2)
                  .filter(pair => pair match
                    case b1::b2::Nil => b2.id == blockToMove.id
                    case _ => false
                  )
                  .toList
                  .head
                  .head
                val withUpdatedFreeSpace = rest.filter(b => b != blockToMove).map(b => if (b.id == preceedingBlock.id) then b.copy(freeSpaceFollowing = b.freeSpaceFollowing + blockToMove.size + blockToMove.freeSpaceFollowing) else b)
                val nextList = leftList ++ List(head.copy(freeSpaceFollowing = 0), blockToMove.copy(freeSpaceFollowing = head.freeSpaceFollowing - blockToMove.size)) ++ withUpdatedFreeSpace
                // println(nextList)
                moveBlocksPt2(nextList, block)
              case None =>
                // println(s"could not find block with id ${blockToMove.id - 1}")
                blocks
          case Nil =>
            nextBlock match
              case Some(block) =>
                // println(s"only place for ${blockToMove.id} is at the end")
                moveBlocksPt2(blocks, block)
              case None =>
                // println("End")
                blocks
    

  @tailrec
  def addStartIndices(processed: List[(Block, Long)], toProcess: List[Block], index: Long): List[(Block, Long)] =
    toProcess match
      case Nil => processed
      case head::tail =>
        addStartIndices(processed ++ List((head, index)), tail, index + head.size + head.freeSpaceFollowing)

  def parsePt1(line: String): Array[Fragment] =
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

  def parsePt2(line: String): List[Block] =
    line
      .toCharArray()
      .toList
      .grouped(2)
      .zipWithIndex
      .map((pair, id) =>
        pair match
          case fileSize::freeSpace::Nil =>
            Block(id = id, size = fileSize.asDigit, freeSpaceFollowing = freeSpace.asDigit)
          case fileSize::Nil => // last one
            Block(id = id, size = fileSize.asDigit, freeSpaceFollowing = 0)
          case _ =>
            throw new Error(s"Unexpected data structure at index ${id * 2}")
      )
      .toList

  def totalFileBlocks(blocks: Array[Fragment]) =
    blocks.filter(b => b match
      case File(_) => true
      case Empty() => false
    ).length

  def calcChecksumPt1(blocks: Array[Fragment]) =
    blocks.zipWithIndex.map((f, i) =>
      f match
        case File(id) => i * id
        case Empty() => 0
    ).sum

  def calcChecksumPt2(blocksWithIndices: List[(Block, Long)]) =
    blocksWithIndices
      .map(
        (block, startIndex) => 
          val range = startIndex to (startIndex + block.size.toLong - 1L)
          range.map(i => i * block.id).sum
      )
      .sum

  def part1(lines: List[String]): String =
    val parsed = parsePt1(lines.head)
    val moved = moveBlocksPt1(parsed, totalFileBlocks(parsed))
    calcChecksumPt1(moved).toString()

  def part2(lines: List[String]): String =
    val parsed = parsePt2(lines.head)
    val moved = moveBlocksPt2(parsed, parsed.last)
    val withIndices = addStartIndices(List.empty, moved, 0)
    
    calcChecksumPt2(withIndices).toString()
}
