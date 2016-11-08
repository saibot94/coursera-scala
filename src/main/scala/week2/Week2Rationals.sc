println("welcome to week 3")
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")
  def this(x: Int) = this(x,1)
  private def gcd(a: Int, b: Int) : Int = if(b==0) a else gcd(b, a%b)
  private val g = gcd(x,y)
  val numer = x / g
  val denom = y / g
  def add(r: Rational) =
    new Rational(r.numer * denom + numer * r.denom, r.denom * denom)
  def sub(r: Rational) =
    add(r.neg)
  def neg: Rational =
    new Rational(-numer, denom)
  override def toString =
    numer + "/" + denom
  def less(that: Rational): Boolean = this.numer * that.denom < that.numer * this.denom
  def max(that: Rational) = if(this.less(that)) this else that
}
val x = new Rational(1, 3)
val y = new Rational(5,7)
val z = new Rational(3, 2)
x.numer
x.denom

x.add(y)
z.max(x)
x.sub(y).sub(z)
y.add(y)

val strange = new Rational(1)
strange.add(strange)
