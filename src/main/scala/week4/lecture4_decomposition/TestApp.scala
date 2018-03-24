package week4.lecture4_decomposition

object TestApp extends App {
  val s1 = Sum(Number(2), Number(3))
  val s2 = Sum(Number(5), Number(6))
  val s3 = Sum(s1, s2)
  val s4: Sum = Sum(s3, Number(10))
//  println(s4.show())

  val exp1 = Sum(Prod(Number(2), Var("x")), Var("y"))
  val exp2 = Prod(Sum(Number(2), Var("x")), Var("y"))
  println(exp1.show())
  println(exp2.show())
}
