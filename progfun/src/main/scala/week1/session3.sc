def abs(x: Double): Double = if(x < 0) -x else x

def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double = {
    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) < 0.001 * x

    def improve(guess: Double) =
      (guess + x / guess) / 2

    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))
  }

  sqrtIter(1)
}

sqrt(4)
sqrt(9)
sqrt(1.0e-6)
sqrt(0.1e-20)
sqrt(1.0e20)
sqrt(1.0e50)
sqrt(1.0e80)