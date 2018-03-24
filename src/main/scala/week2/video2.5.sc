val x = new Rational(1, 3)
val y = new Rational(1,2)
x - y toString



class Rational(x: Int, y: Int) {
  require(y != 0, "denominator has to be non 0")

  def this(x: Int) = this(x, 1)

  def numer = x
  def denom = y

  private val g = gcd(x, y)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def -(that: Rational) = this + -that

  override def toString = numer/g + "/" + denom/g

}


