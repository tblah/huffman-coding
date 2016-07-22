import huffman._

object main {
  def main(args: Array[String]): Unit = {
    val tree = CodeTree("test".toList)

    println(tree.toString)
    println("code for t is " + tree.codingTreeLookup('t').toString())
    println("code for e is " + tree.codingTreeLookup('e').toString())
    println("code for \"te\" is " + tree.encodeString("te").toString())
  }
}