val h = Vector(1,2,3)
h
h :+ 37

val r = Array(1,2,3,44)
val r2 = Array(3,4,5,55)

r zip r2 unzip

r map (x => x * 2)

val s = "Hello World"
s exists (c => c.isUpper)
s forall (c => c.isUpper)

val s2 = "hello smthr"

val M = 3
val N = 3
(1 to M) flatMap( x =>
  (1 to N) map (y => (x,y))
  )

def scalarProd(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map { case (x,y) => x * y}.sum

def scalarProd2(xs: Vector[Double], ys:Vector[Double]) : Double =
  (for (
    (x,y) <- xs zip ys
  ) yield x * y ).sum

def isPrime(n: Int): Boolean =
  (2 until n) forall (nr => n % nr != 0)

def primePairs(n: Int): IndexedSeq[(Int,Int)] =
  (1 until n) flatMap (i =>
     (1 until i) map (j => (i, j))) filter (pair =>
      isPrime(pair._1 + pair._2)
    )

def primePairsFor(n:Int) : IndexedSeq[(Int,Int)] =
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i,j)

isPrime(2)
isPrime(3)
isPrime(19)
isPrime(20)


primePairs(5)

primePairsFor(5)

// n-queens

def queens(n:Int):Set[List[Int]] = {
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row  - 1  to 0 by -1) zip queens
    queensWithRow.forall {
      case (r,c) => col != c && math.abs(col - c) != row - r
    }
  }

  def placeQueens(k: Int) : Set[List[Int]] =
    if(k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens

  placeQueens(n)
}
def show(q: List[Int]) = {
  val lines =
    for(l <- q.reverse)
      yield Vector.fill(q.length)("* ").updated(l, "X ").mkString
  "\n" + (lines mkString "\n")
}

(queens(4)  map show) mkString "\n"


