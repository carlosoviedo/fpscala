def abs(x: Double): Double = if(x < 0) -x else x

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double) =
  abs(guess * guess - x) < 0.001 * x

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def sqrt(x: Double) = sqrtIter(1, x)

sqrt(4)
sqrt(9)
sqrt(1.0e-6)
sqrt(0.1e-20)
sqrt(1.0e20)
sqrt(1.0e50)
sqrt(1.0e80)