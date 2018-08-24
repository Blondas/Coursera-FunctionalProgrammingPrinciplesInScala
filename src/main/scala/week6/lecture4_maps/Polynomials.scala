package week6.lecture4_maps

object Polynomials extends App{

  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms: Map[Int, Double] = terms0 withDefaultValue 0.0

//    def + (other: Poly)= new Poly(terms ++ other.terms.map(adjust))
    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> ((coeff) + terms(exp)))
    }

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def toString = (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }

  val p1 = new Poly(1 -> 2, 3 -> 4, 5 -> 6)
  val p2 = new Poly(0 -> 3, 3 -> 7)

  println(p1)
  println(p2)
  println (p1 + p2)
}
