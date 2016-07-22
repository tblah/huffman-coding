import huffman._

object main {
  def main(args: Array[String]): Unit = {
    val msg = "test message"
    
    val tree = CodeTree(msg.toList) // this does not have to be msg but it needs to include all the characters used in msg

    val code = tree.encodeString(msg)
    
    println(tree.decode( code ))
    
    //msg.foreach((c: Char) => println("The code for " + c + " is " + tree.codingTreeLookup(c).toString))
    
    //println("The tree was " + tree.toString())
    //println("The code was " + code.toString())
    
  }
}