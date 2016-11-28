abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat  = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero = true

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if(that.isZero) this else throw new Error("0.minus")

  override def predecessor: Nat = throw new Error("0.predecessor")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if(that.isZero) this else n - that.predecessor

  override def predecessor: Nat = n
}

val n1 = new Succ(new Succ(Zero))
val n2 = new Succ(Zero)

n1 - n2


