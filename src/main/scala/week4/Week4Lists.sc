val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
diag3.foreach(l => {
  l.foreach(item =>
    print(item)
  )
  println()
})


val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
fruit.head

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (y < x) y :: insert(x, ys) else x :: xs
}

def isort(xs: List[Int]) : List[Int] = xs match {
  case List() => List()
  case x :: xs => insert(x, isort(xs))
}

isort(List(4,3,1,7,9))
insert(1, insert(8, insert(6, List(1, 2, 3, 4))))