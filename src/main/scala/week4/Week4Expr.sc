trait Expr {
  def isNumber: Boolean
  def isSum :Boolean
  def leftOp: Expr
  def rightOp: Expr
  def numValue: Int
}

class Sum(l: Expr, r: Expr) extends Expr {
  override def isNumber: Boolean = false
  override def numValue: Int = throw new Error("Sum.numValue")
  override def isSum: Boolean = true
  override def leftOp: Expr = l
  override def rightOp: Expr = r
}

class Num(n: Int) extends Expr {
  override def isNumber: Boolean = true

  override def isSum: Boolean = false

  override def leftOp: Expr = throw new Error("Num.leftOp")

  override def rightOp: Expr = throw new Error("Num.rightOp")

  override def numValue: Int = n
}


def eval(e: Expr): Int = {
  if(e.isNumber) e.numValue
  else if(e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unknown expression " + e)
}

eval(new Sum(new Num(1), new Num(2)))