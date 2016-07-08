object session {


  def factorial(x: Int): Int = {
    def factorialAux(current: Int, total: Int): Int = {
      if (current == 0) {
        total
      } else {
        factorialAux(current - 1, current * total)
      }
    }
    factorialAux(x, 1)
  }

  factorial(5)


  def abs(x: Double) = if (x >= 0) x else -x

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(4)
  sqrt(2)
  sqrt(1e-6)
  sqrt(1e60)


}
