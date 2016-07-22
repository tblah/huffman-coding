import huffman._

object main {
  def main(args: Array[String]): Unit = {
    val tree = CodeTree( "testing testing can you compress me?".toList )
    
    println( tree.toString )
  }
}