println("welcome to week 3")
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")
  def this(x: Int) = this(x,1)
  private def gcd(a: Int, b: Int) : Int = if(b==0) a else gcd(b, a%b)
  private val g = gcd(x,y)
  val numer = x / g
  val denom = y / g
  def +(that: Rational) =
    new Rational(that.numer * denom + numer * that.denom, that.denom * denom)
  def -(that: Rational) =
    this + (-that)
  def unary_- : Rational =
    new Rational(-numer, denom)
  override def toString =
    numer + "/" + denom
  def < (that: Rational): Boolean = this.numer * that.denom < that.numer * this.denom
  def max(that: Rational) = if(this < that) this else that
}
val x = new Rational(1, 3)
val y = new Rational(5,7)
val z = new Rational(3, 2)
x.numer
x.denom

x + y
z max x
x - y  - z
y + y

val strange = new Rational(1)
strange + strange

-strange
