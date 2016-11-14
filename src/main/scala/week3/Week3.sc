abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet) : IntSet
}

object Empty extends IntSet {
  override def incl(x: Int): IntSet =
    new NonEmpty(x, Empty, Empty)

  override def contains(x: Int): Boolean = false


  override def toString: String =
    "."

  override def union(other: IntSet): IntSet = other
}

class NonEmpty(value: Int, left: IntSet,
               right: IntSet) extends IntSet {
  override def incl(x: Int): IntSet =
    if(x < value) new NonEmpty(value, left incl x, right)
    else if(x > value) new NonEmpty(value, left, right incl x)
    else this


  override def contains(x: Int): Boolean =
    if(x < value) left contains x
    else if (x > value) right contains x
    else true

  override def toString: String =
    "{"  + left + value + right + "}"

    override def union(other: IntSet): IntSet =
      ((left union right) union other) incl value
}


// impls
val nonempty1 = new NonEmpty(3, new NonEmpty(2, Empty, Empty),
  new NonEmpty(4, Empty, Empty))

val nonempty2 = nonempty1 incl 6

abstract class Base {
  def foo = 1
  def bar : Int
}

class Sub extends Base {
  def bar: Int = 3
  override def foo = 3
}
