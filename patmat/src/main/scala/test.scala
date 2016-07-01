import patmat.Huffman
import patmat.Huffman._

/**
  * Created by "Threezj" on 16-6-30.
  */
object test {
    def main(args: Array[String]) {
        println(encode(frenchCode)("xaxa".toList))
        println(quickEncode(Huffman.frenchCode)("xaxa".toList))
    }
}
