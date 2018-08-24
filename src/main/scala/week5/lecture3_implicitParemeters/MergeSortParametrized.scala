package week5.lecture3_implicitParemeters

object MergeSort {

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2

    if (n == 0) xs else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) if ord.lt(x,y) => x :: merge(xs1, ys)
        case (x :: xs1, y :: ys1) => y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort(fst)(ord), msort(snd)(ord))
    }
  }
}

object Test extends App {
  val l1 = List(3,5,12,-3,0)
  val fruits = List("apple", "pineapple", "banana", "orange")

  println {
    MergeSort.msort(l1)
  }
  println {
    MergeSort.msort(fruits)
  }
}
