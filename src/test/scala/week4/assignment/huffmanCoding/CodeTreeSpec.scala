package week4.assignment.huffmanCoding

import org.scalatest.{FlatSpec, Matchers}

class CodeTreeSpec extends FlatSpec with Matchers {
  import CodeTreeSpecHelper._
  import CodeTree._

  "weight" should "calculate node weight correctly" in {
    weight(c) shouldEqual 1
    weight(cd) shouldEqual 2
    weight(abcdefgh) shouldEqual 17
  }

  "chars" should "chars list correctly correctly" in {
    chars(c) shouldEqual List(c.char)
    chars(cd) shouldEqual cd.chars
    chars(abcdefgh) shouldEqual abcdefgh.chars
  }

}

object CodeTreeSpecHelper {
  val c = Leaf('c', 1)
  val d = Leaf('d', 1)
  val e = Leaf('e', 1)
  val f = Leaf('f', 1)
  val g = Leaf('g', 1)
  val h = Leaf('h', 1)

  val b = Leaf('b', 3)
  val cd = Fork(c, d, "cd".toList, 2)
  val ef = Fork(e, f, "ef".toList, 2)
  val gh = Fork(g, h, "gh".toList, 2)

  val bcd = Fork(b, cd, "bcd".toList, 5)
  val efgh = Fork(ef, gh, "efgh".toList, 4)

  val a = Leaf('a', 8)
  val bcdefgh = Fork(bcd, efgh, "bcdefgh".toList, 9)

  val abcdefgh = Fork(a, bcdefgh, "abcdefgh".toList, 17)
}




