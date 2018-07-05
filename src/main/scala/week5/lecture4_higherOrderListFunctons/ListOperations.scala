package week5.lecture4_higherOrderListFunctons

object ListOperations extends App{

  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => y * y :: squareList(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (e => e * e)

}
