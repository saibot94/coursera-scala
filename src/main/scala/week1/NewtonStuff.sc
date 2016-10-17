import scala.annotation.tailrec

object session {
  1 + 2

  def abs(x: Double) = if (x < 0) -x else x


  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2
    sqrtIter(1.0)
  }

  sqrt(2)
  sqrt(3)
  val x = sqrt(1e-6) + 3


  def factorial(x: Int): Int= {
    @tailrec
    def tailRecFactorialAcc(res: Int, x: Int): Int= {
      if (x <= 1)
        res
      else
        tailRecFactorialAcc(res * x, x - 1)
    }
    tailRecFactorialAcc(1, x)
  }


  def normalFactorial(x: Int) : Int= {
    if(x == 0){
      1
    } else {
      x * normalFactorial(x-1)
    }
  }

  factorial(2)
  factorial(3)
  factorial(5)
  factorial(4)
}
