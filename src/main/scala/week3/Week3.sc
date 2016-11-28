import java.util.NoSuchElementException

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

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

val x = null
val y : String = x
if(true) 1 else false

// cons lists

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail : List[T]) extends List[T]{
  override def isEmpty = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
}

def singleton[T](item: T) = new Cons[T](item, new Nil[T])
singleton(3)
singleton(true)


def get[T](n : Int, list: List[T]) : T = {
  if (list isEmpty) throw new IndexOutOfBoundsException("element not found")
  if (n == 0) {
    list.head
  } else {
    get(n - 1, list.tail)
  }
}


val cons = new Cons(1, new Cons(2, new Cons(3, new Nil)))
get(2, cons)