package week5.lecture2_pairs_and_Tuples

object MergeSort {

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2

    if (n == 0) xs else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) if x < y => x :: merge(xs1, ys)
        case (x :: xs1, y :: ys1) => y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }
}

object Test extends App {
  val l1 = List(1,3,5,5,8,12)
  val l2 = List(2,2,4,5,9)
  println {
    MergeSort.merge(l1, l2)
  }
}
