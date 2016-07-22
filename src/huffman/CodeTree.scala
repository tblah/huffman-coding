package huffman
import scala.annotation.tailrec


// tree representation
abstract class CodeTree {
  def weight: Int
  def chars: List[Char]
}

case class Fork(left: CodeTree, right: CodeTree) extends CodeTree {
  def weight: Int = left.weight + right.weight
  def chars: List[Char] = left.chars ++ right.chars
}

case class Leaf(char: Char, weight: Int) extends CodeTree {
  def chars: List[Char] = List(char)
}


//

// tree creation
object CodeTree {
  def apply(chars: List[Char]): CodeTree = {

    def constructTree = until(singleton, combine)

    val leaves = makeOrderedLeafList(times(chars))

    constructTree(leaves).head
  }

  // count the number of times each char appears in chars. Unsorted
  private def times(chars: List[Char]): List[(Char, Int)] = {

    @tailrec def times_iter(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {

      // update char in the acc list
      def updateAcc(c: Char, acc: List[(Char, Int)]): List[(Char, Int)] = {
        @tailrec def updateAccIter(source: List[(Char, Int)], processed: List[(Char, Int)]): List[(Char, Int)] = {
          if (source.isEmpty) (c, 1) +: acc // character was not found so prepend to list (prepend is faster than append)
          else if (source.head._1 == c) processed ::: List((c, source.head._2 + 1)) ::: source.tail // cound character in list. Add 1 to it's count
          else updateAccIter(source.tail, processed ::: List(source.head)) // char not found. Continue itterating
        }

        updateAccIter(acc, Nil)
      } // updateAcc

      // times_iter
      if (chars.isEmpty) acc
      else times_iter(chars.tail, updateAcc(chars.head, acc)) // for each char in chars updateAcc
    }
    times_iter(chars, Nil)
  }

  // given the result of times, consturct an ascending sorted (by frequency) list of leaves to be added to the huffington tree
  private def makeOrderedLeafList(freqs: List[(Char, Int)]): List[CodeTree] = {
    // make leaf list from a sorted list of freqs
    def makeLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
      @tailrec def iter(freqs: List[(Char, Int)], ret: List[Leaf]): List[Leaf] = {
        if (freqs.isEmpty) ret
        else iter(freqs.tail, ret :+ Leaf(freqs.head._1, freqs.head._2))
      }

      iter(freqs, Nil)
    }

    makeLeafList(freqs.sortWith(_._2 < _._2))
  }

  // is the argument a list of only one CodeTree object?
  private def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  // (1) removes the two trees with the lowest weight from the argument list
  // (2) replaces them with a Fork node
  private def combine(trees: List[CodeTree]): List[CodeTree] = {
    val first = trees.head
    val second = trees.tail.head
    val the_rest = trees.tail.tail
    val reducedList = the_rest :+ Fork(first, second)
    // sort and return
    reducedList.sortWith(_.weight < _.weight)
  }

  // returns a function to repeatedly apply operation until condition is satisfied
  private def until[T](condition: T => Boolean, operation: T => T): T => T = {
    @tailrec def repeat(arg: T): T =
      if (condition(arg)) arg
      else repeat(operation(arg))

    repeat
  }

} // object CodeTree