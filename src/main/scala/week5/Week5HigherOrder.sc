def scaleList(xs: List[Double], factor: Double): List[Double] = xs map (x => x * factor)


scaleList(List(1, 2, 3, 4), 0.5)


def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

def squareListMap(xs: List[Int]): List[Int] =
  xs map (x => x * x)

def posElems(xs: List[Int]): List[Int] =
  xs filter (x => x > 0)

squareList(List(1, 2, 3, 4))
squareListMap(List(1, 2, 3, 4))

posElems(List(1, 2, 3, -5, -6, 10))



val nums = List(-2, 1, 3, 5, 6)
nums filter (x => x > 0)
nums filterNot (x => x > 0)

nums partition (x => x > 0)

nums takeWhile (x => x < 0)
nums dropWhile (x => x < 0)
nums span (x => x < 0)


// packing function
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs span(y => y == x)
    first :: pack(rest)
  }
}

// or
def pack2[T](xs: List[T]): List[List[T]] = xs match  {
  case Nil => Nil
  case x :: xs1 => (x :: xs1 takeWhile (y => y == x)) :: pack2(xs1 dropWhile (y => y == x))
}

// RLE
def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs span (y => y == x)
    val head= (x, first.length)
    head :: encode(rest)
  }
}
// or

def encode2[T](xs: List[T]): List[(T,Int)] = pack(xs) map (x => (x.head, x.length))


val someList = List("a", "a", "a", "b", "b", "c")

pack(someList)
pack2(someList)

encode(someList)
encode2(someList)


nums.reduceLeft(_ + _)
(nums foldLeft 0)(_ + _)


(nums foldLeft 1)(_ * _)


def concat[T](x: List[T], y: List[T]): List[T] =
  (x foldRight y)(_ :: _)

concat(List(1,2,3), List(4,5,6))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((x,y) => f(x) :: y)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((i, z) => z + 1)

lengthFun(List(1,2,3,4,5))
mapFun[Int,Int](List(1,2,3,4,5), x => x * 2)