abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

class Empty extends IntSet {
  override def incl(x: Int): IntSet =
    new NonEmpty(x, new Empty, new Empty)

  override def contains(x: Int): Boolean = false


  override def toString: String =
    "."

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

}


// impls
val nonempty1 = new NonEmpty(3, new NonEmpty(2, new Empty, new Empty),
  new NonEmpty(4, new Empty, new Empty))

val nonempty2 = nonempty1 incl 6
