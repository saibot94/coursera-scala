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

class Poly(val terms: Map[Int,Double]) {
  private def adjust(term: (Int,Double)): (Int,Double) =
    terms get term._1 match {
      case Some(otherVal) => (term._1, term._2 + otherVal)
      case None => term
    }
  def +(other: Poly) = new Poly(terms ++
    (other.terms map adjust))

  override def toString: String =
    (for ((exp, coef) <- terms.toList.sorted.reverse )
      yield coef + "x^" + exp) mkString "+"
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2
