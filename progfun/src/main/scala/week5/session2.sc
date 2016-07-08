def msort(xs: List[Int])(merge: (List[Int], List[Int]) => List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst)(merge), msort(snd)(merge))
  }
}

def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
  case Nil => ys
  case x1 :: xs1 => ys match {
    case Nil => xs
    case y1 :: ys1 =>
      if (x1 < y1) x1 :: merge(xs1, ys)
      else y1 :: merge(xs, ys1)
  }
}

def merge2(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil, Nil) => Nil
    case (xs, Nil) => xs
    case (Nil, ys) => ys
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge2(xs1, ys)
      else y :: merge2(xs, ys1)
  }

val nums = List(2, -4, 5, 7, 1)
msort(nums)(merge)
msort(nums)(merge2)