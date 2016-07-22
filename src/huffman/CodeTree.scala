package huffman
import scala.annotation.tailrec

// tree representation
abstract class CodeTree {
  def weight: Int
  def chars: List[Char]
  def codingTreeLookup(c: Char): List[Boolean]

  def encodeString(s: String): List[Boolean] = {
    @tailrec def iter(list: List[Char], acc: List[Boolean]): List[Boolean] = list match {
      case List()       => acc
      case head :: tail => iter(tail, acc ++ codingTreeLookup(head))
    }

    iter(s.toList, Nil)
  }

  def decode(code: List[Boolean]): String = {
    decodeIter(this, "", code)
  }

  // not to be used by external programs but this has to be public so that leaves can call this on the root
  def decodeIter(root: CodeTree, out: String, code: List[Boolean]): String
}

case class Fork(left: CodeTree, right: CodeTree) extends CodeTree {
  def weight: Int = left.weight + right.weight
  def chars: List[Char] = left.chars ++ right.chars

  // taking a left branch is 0, taking a right branch is 1
  def codingTreeLookup(c: Char): List[Boolean] = {
    if (!(chars.contains(c))) throw new IllegalArgumentException // c is not in this tree

    // first try going left
    try {
      List(false) ++ left.codingTreeLookup(c)
    } catch { // c was not found in the left subtree so look in the right
      case ex: IllegalArgumentException => List(true) ++ right.codingTreeLookup(c)
    }
  }

  def decodeIter(root: CodeTree, out: String, code: List[Boolean]): String = {
    if (code isEmpty) throw new Error("Incomplete hamming code: ran out of code on a Fork in the hamming tree")

    if (code.head)
      right.decodeIter(root, out, code.tail)
    else
      left.decodeIter(root, out, code.tail)
  }
}

case class Leaf(char: Char, weight: Int) extends CodeTree {
  def chars: List[Char] = List(char)
  def codingTreeLookup(c: Char): List[Boolean] =
    if (char == c) Nil // the Fork will have added the correct bit
    else throw new IllegalArgumentException // we did not find c

  def decodeIter(root: CodeTree, out: String, code: List[Boolean]): String = {
    if (code isEmpty) out + char
    else root.decodeIter(root, out + char, code)
  }
}

//

// tree creation
object CodeTree {
  def apply(chars: String): CodeTree = {

    def constructTree = until(singleton, combine)

    val leaves = makeOrderedLeafList(times(chars.toList))

    constructTree(leaves).head
  }

  // count the number of times each char appears in chars. Unsorted
  private def times(chars: List[Char]): List[(Char, Int)] = {
    @tailrec def times_iter(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
      // update the acc list with this occurrence of this character in chars
      def updateAcc(c: Char, acc: List[(Char, Int)]): List[(Char, Int)] = {
        // search the acc list for this char and amend as needed
        @tailrec def updateAccIter(source: List[(Char, Int)], processed: List[(Char, Int)]): List[(Char, Int)] = source match {
          // character was not found so prepend to list (prepend is faster than append)
          case List() => (c, 1) +: processed
          // non-empty list
          case (char, count) :: tail => {
            if (char == c)
              // found character in list. Construct a new list with this character's count incremented
              processed ::: List((c, count + 1)) ::: tail
            else // char not found but there is list left to check through
              updateAccIter(tail, processed ::: List((char, count)))
          }
        }

        updateAccIter(acc, Nil)
      }

      // times_iter
      chars match {
        case List() => acc
        case head :: tail => times_iter( tail, updateAcc(head, acc))
      }
    }
    
    times_iter(chars, Nil)
  }

  // given the result of times, construct an ascending sorted (by frequency) list of leaves to be added to the huffington tree
  private def makeOrderedLeafList(freqs: List[(Char, Int)]): List[CodeTree] = {
    // make leaf list from a sorted list of frequencies
    def makeLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
      @tailrec def iter(freqs: List[(Char, Int)], ret: List[Leaf]): List[Leaf] = freqs match {
        case List() => ret
        case (char, num) :: tail => iter(tail, ret :+ Leaf(char, num))
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