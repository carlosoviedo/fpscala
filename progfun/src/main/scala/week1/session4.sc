import scala.annotation.tailrec

def factorial(n: Int) = {
  @tailrec
  def loop(currentN: Int, result: Int): Int =
    if (currentN == 0) result
    else loop(currentN - 1, currentN*result)

  loop(n, 1)
}

factorial(5)
factorial(6)