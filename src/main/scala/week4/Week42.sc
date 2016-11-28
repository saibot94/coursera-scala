
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
  override def toString = {
    def toStringAcc(list :List[T], acc: StringBuilder): String = {
      if(list.isEmpty) {
        acc.toString()
      } else {
        acc.append(list.head + " ")
        toStringAcc(list.tail, acc)
      }
    }
    toStringAcc(this, new StringBuilder)
  }

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

object List {
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))
  def apply[T](x1: T): List[T] = new Cons(x1, Nil)
  def apply[T](): List[T] = Nil
}

object test {
  val x: List[String] = Nil
}


val myList = List(1,2)
myList.prepend(new Cons(4, Nil))

def f(xs: List) = xs prepend Nil


