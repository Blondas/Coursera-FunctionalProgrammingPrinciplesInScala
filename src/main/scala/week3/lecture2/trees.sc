import sun.invoke.empty.Empty

abstract class IntSet {
  def add(x: Int): lecture2.IntSet
  def contains(x: Int): Boolean
}


object Empty extends lecture2.IntSet {
  override def add(x: Int): lecture2.IntSet = new lecture2.NonEmpty(x, Empty, Empty)
  override def contains(x: Int): Boolean = false
  override def toString = "."
}


class NonEmpty(node: Int, left: lecture2.IntSet, right: lecture2.IntSet) extends lecture2.IntSet {
  override def add(x: Int): lecture2.IntSet =
    if (x < node) new lecture2.NonEmpty(node, left add x, right)
    else if (x > node) new lecture2.NonEmpty(node, left, right add x)
    else this

  override def contains(x: Int): Boolean =
    if (x < node) left contains x
    else if (x > node) right contains x
    else true

  override def toString = "{" + left + node + right + "}"
}


val t1 = new lecture2.NonEmpty(3, Empty, Empty)
val t2 = t1 add 4