package idealized.scala

abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => Boolean): Boolean = ifThenElse(x, `false`)
  def || (x: => Boolean): Boolean = ifThenElse(`true`, x)
  def unary_! : Boolean = ifThenElse(`false`, `true`)

  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)

  // zalozenie: false < true
  def < (x: Boolean): Boolean = ifThenElse(`false`, x)
}

object `true` extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = t
}

object `false` extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = e
}

object A extends App {
  val a = `true`.<(`true`)
  val b = `true`.<(`false`)
  val c = `false`.<(`false`)
  val d = `false`.<(`true`)
}

