val capitals = Map("US" -> "Washington", "Switzerland" -> "Bern")

val res: Option[String] = capitals get "US"

def showCapital(country: String) = capitals get country match {
  case Some(capital) => capital
  case None => "missing data"
}

showCapital("US")

val fruit = List("apple", "pear", "orange", "pineapple")
fruit sortWith (_.length < _.length)

fruit groupBy (_.head)

class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int,Double)*) = {
    this(bindings.toMap)
  }

  val terms = terms0 withDefaultValue 0.0
  private def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp,coeff) = term
    exp -> (terms(exp) + coeff)
  }

  def +(other: Poly) = new Poly(terms ++
    (other.terms map adjust))

  override def toString: String =
    (for ((exp, coef) <- terms.toList.sorted.reverse)
      yield coef + "x^" + exp) mkString "+"
}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
p1 + p2
