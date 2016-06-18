def isort(xs: scala.List[Int]): scala.List[Int] = {
  def insert(y: Int, ys: scala.List[Int]): scala.List[Int] = {
    ys match {
      case scala.Nil => y :: scala.Nil
      case z :: zs =>
        if (y < z) y :: ys
        else z :: insert(y, zs)
    }
  }

  xs match {
    case scala.Nil => xs
    case y :: ys => insert(y, isort(ys))
  }
}

val x = scala.List(1,5,3,8,7,2,10)
isort(x)