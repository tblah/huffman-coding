import huffman._

object main {
  def main(args: Array[String]): Unit = {
    val msg = "test message. Plus a load of other stuff. Hopefully I can find bugs by brute force. One two three four five six seven eight nine ten. abcdefghijklmnopqrstuvwxyz"
    
    val tree = CodeTree(msg) // this does not have to be msg but it needs to include all the characters used in msg

    val code = tree.encodeString(msg)
    
    println(tree.decode( code ))
    
    //msg.foreach((c: Char) => println("The code for " + c + " is " + tree.codingTreeLookup(c).toString))
    
    //println("The tree was " + tree.toString())
    //println("The code was " + code.toString())
    
  }
}