val tolerance = 0.0001

def abs(x: Double): Double = if(x < 0) -x else x

def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x ) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    println(s"guess: $guess, next: $next")
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) =
  (x + f(x)) / 2

def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1)

fixedPoint(x => 1 + x/2)(1)
sqrt(2)
sqrt(4)
