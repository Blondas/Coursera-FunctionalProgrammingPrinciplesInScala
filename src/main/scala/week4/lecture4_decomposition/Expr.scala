package week4.lecture4_decomposition

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1,e2) => e1.eval + e2.eval
  }

  def show(e: Expr = this): String = e match {
    case Var(x) => x
    case Number(n) => n.toString
    case Prod(e1, e2) => (e1, e2) match {
      case (Sum(_,_), Sum(_,_)) => "(" + e1.show() + ")" + " * " + "(" + e2.show() + ")"
      case (Sum(_,_), _) => "(" + e1.show() + ")" + " * " + e2.show()
      case (_, Sum(_,_)) => e1.show() + " * " + "(" + e2.show() + ")"
      case _ => e1.show() + " * " + e2.show()
    }
    case Sum(e1, e2) => e1.show() + " + " + e2.show()
    case _ => throw new Error("expression not implemented yet")
  }
}
