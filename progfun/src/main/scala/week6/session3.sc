def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(Nil)
    else
      for {
        queens <- placeQueens(k-1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val rows = queens.length
    (rows-1 to 0 by -1) zip queens forall {
      case (row, pos) => col != pos && math.abs(col - pos) != rows - row
    }
  }

  placeQueens(n)
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

val sol8 = queens(8)
sol8.size

val sol4 = queens(4)
sol4.size
(sol4 map show) mkString "\n"