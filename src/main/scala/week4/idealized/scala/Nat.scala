// Peano numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  override def predecessor: Nat = throw new NoSuchElementException("0.predecessor")
  override def +(that: Nat): Nat = that
  override def -(that: Nat): Nat = if (that.isZero) Zero
    else throw new NoSuchElementException("0.-")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false
  override def predecessor: Nat = n
  override def +(that: Nat): Nat = new Succ(n + that)
  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}

