package week4.lecture7_lists

class InsertionSort {
  def iSort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, iSort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
}