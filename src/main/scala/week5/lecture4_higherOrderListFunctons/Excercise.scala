package week5.lecture4_higherOrderListFunctons

object Excercise extends App {
  val list = List("a", "a", "a", "b", "c", "c", "a")
  val expected = List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs.span(_ == x)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map(l => (l.head, l.length))

  println {
    pack(list)
  }

  println {
    encode(list)
  }
}
