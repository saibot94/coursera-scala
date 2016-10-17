val x = 3
def sum(f : Int => Int)(a: Int, b: Int): Int =
  if(a > b) 0
  else f(a) + sum(f)( a + 1, b)

def cube = (x:Int) => x * x * x
def sumInts(a: Int, b: Int) = sum((x: Int) => x)(a, b)
def sumCubes(a: Int, b: Int) = sum(cube)(a, b)

sumInts(1,5)
sumCubes(1, 4)

def sumTailrec(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc
    else {
      loop(a + 1, acc + f(a))
    }
  }
  loop(a, 0)
}

sumTailrec(x => x*x)(1, 5)