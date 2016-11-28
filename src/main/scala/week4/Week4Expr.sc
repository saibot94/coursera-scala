trait Expr {
  //  def isNumber: Boolean
  //  def isSum :Boolean
  //  def leftOp: Expr
  //  def rightOp: Expr
  //  def numValue: Int
  //  def eval: Int
  def eval: Int = this match {
    case Num(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Product(e1, e2) => e1.eval * e2.eval
  }

  def show: String = this match {
    case Num(n) => n.toString
    case Sum(e1, e2) => e1.show + " + " + e2.show
    case Product(e1, e2) =>  {
      val str1 = e1 match {
        case Sum(_, _) => "( "  + e1.show + ")"
        case _ => e1.show
      }
      val str2 = e2 match {
        case Sum(_, _) => "( "  + e2.show + ")"
        case _ => e2.show
      }
      str1 + " * " + str2
    }
    case Var(x) => x
  }
}

case class Sum(l: Expr, r: Expr) extends Expr {
  //  override def isNumber: Boolean = false
  //  override def numValue: Int = throw new Error("Sum.numValue")
  //  override def isSum: Boolean = true
  //  override def leftOp: Expr = l
  //  override def rightOp: Expr = r
  //  override def eval: Int = leftOp.eval + rightOp.eval
}

case class Product(l: Expr, r: Expr) extends Expr

case class Num(n: Int) extends Expr {
  //  override def isNumber: Boolean = true
  //
  //  override def isSum: Boolean = false
  //
  //  override def leftOp: Expr = throw new Error("Num.leftOp")
  //
  //  override def rightOp: Expr = throw new Error("Num.rightOp")
  //
  //  override def eval: Int = numValue
  //
  //  override def numValue: Int = n
}

case class Var(x: String) extends Expr


def eval(e: Expr): Int = e match {
  case Num(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Product(e1, e2) => eval(e1) * eval(e2)
}

val s = Product(Sum(Num(3), Num(4)), Sum(Num(1), Var("x")))
s.show

val test1 = Sum(Product(Num(2), Var("x")), Var("y"))
test1.show

val test2 = Product(Sum(Num(2), Var("x")), Var("y"))
test2.show