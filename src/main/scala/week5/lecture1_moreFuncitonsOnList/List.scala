package week5.lecture1_moreFuncitonsOnList

object Lista extends App {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case x :: xs => x :: init(xs)
  }

//  def concat[T](xs: List[T], ys: List[T]) = xs match {
//    case List() => ys
//    case z :: zs => z :: concat(zs, ys)
//  }

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case (y :: ys) :: zs => y :: flatten(ys ::: zs)
    case y :: ys => y :: flatten(ys)

  }
  println {
    flatten(List(List(1, 1), List(3, List(5, 8))))
  }
}

