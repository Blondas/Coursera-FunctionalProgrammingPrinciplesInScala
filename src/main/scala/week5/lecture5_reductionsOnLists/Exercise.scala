package week5.lecture5_reductionsOnLists

object Exercise extends App{
  def mapFun[T,U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((x, y) => f(x) :: y)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ((x, y) => y + 1 )

  println{
    mapFun[Int, Int](List(1,2), _ * 2)
  }

  println{
    lengthFun[Int](List(1,2,3,4))
  }
}
