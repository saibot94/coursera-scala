val ls = List(1, 2, 3)
ls indexOf 1

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

last(ls)

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case x :: ys => x :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => n match {
    case 0 => ys
    case _ => y :: removeAt(n - 1, ys)
  }
}

init(ls)

reverse(concat(List(1, 2, 3, 4), List(5, 7, 8)))
removeAt(1, List('a', 'b', 'c', 'd'))


def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def mergeAcc(xs: List[T], ys: List[T], acc: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => acc ::: ys
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) mergeAcc(xs1, ys, acc ::: List(x))
          else mergeAcc(xs, ys1, acc ::: List(y))
        case (_, Nil) => acc ::: xs
      }

    val (fst, snd) = xs splitAt n
    mergeAcc(msort(fst), msort(snd), List())
  }
}

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) / 1000000.0 + "ms")
  result
}

var res = List[Int]()
for (i <- 1 to 10000) {
  res = i :: res
}
time {
  msort(res)
}

val fruits = List("apple", "pineapple", "orange", "banana")
msort(fruits)

