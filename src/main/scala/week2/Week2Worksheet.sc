def sum(f : Int => Int)(a: Int, b: Int): Int =
  if(a > b) 0
  else f(a) + sum(f)( a + 1, b)

def cube = (x:Int) => x * x * x
def sumInts(a: Int, b: Int) = sum((x: Int) => x)(a, b)
def sumCubes(a: Int, b: Int) = sum(cube)(a, b)

def sumTailrec(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc
    else {
      loop(a + 1, acc + f(a))
    }
  }
  loop(a, 0)
}

def sumFromF(f:Int => Int) : (Int, Int) => Int = {
  def sumF(a: Int, b: Int ): Int=
    if(a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}

def sumCurry(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 0 else f(a) + sumCurry(f)(a+1, b)
}


sumInts(1,5)
sumCubes(1, 4)

sumTailrec(x => x*x)(1, 5)
sumCurry(x => x * x)(1, 5)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if(a > b) 1 else f(a) * product(f)(a+1,b)

def factorial(n: Int) =
  product(x => x)(1, n)

factorial(3)
factorial(4)

def mapReduce(f : Int => Int, combine: (Int, Int) => Int, zero : Int)(a: Int, b: Int): Int =
  if(a > b) zero
else combine(f(a), mapReduce(f, combine, zero)(a+1,b))

def productOp(a: Int, b: Int) = mapReduce((x: Int) => x , (x: Int,y: Int)  => x * y, 1)(a,b)
productOp(1, 5)
