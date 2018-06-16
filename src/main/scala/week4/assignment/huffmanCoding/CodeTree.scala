package week4.assignment.huffmanCoding

abstract class CodeTree {


}
case class Fork (left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

object CodeTree {
  // total weight of a given Huffman tree.def weight(tree: CodeTree): Int = tree match ...
  def weight(tree: CodeTree): Int = tree match {
    case t: Fork => weight(t.left) + weight(t.right)
    case l: Leaf => l.weight
  }

  // list of characters defined in a given Huffman tree.def chars(tree: CodeTree): List[Char] = tree match ...
  def chars(tree: CodeTree): List[Char] = tree match {
    case t: Fork => chars(t.left) ++ chars(t.right)
    case l: Leaf => List(l.char)
  }
}