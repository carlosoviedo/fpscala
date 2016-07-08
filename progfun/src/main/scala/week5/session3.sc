def msort[T](xs: List[T])(merge: (List[T], List[T]) => List[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst)(merge), msort(snd)(merge))
  }
}

def merge[T](lessThan: (T, T) => Boolean)(xs: List[T], ys: List[T]): List[T] = xs match {
  case Nil => ys
  case x1 :: xs1 => ys match {
    case Nil => xs
    case y1 :: ys1 =>
      if (lessThan(x1,y1)) x1 :: merge(lessThan)(xs1, ys)
      else y1 :: merge(lessThan)(xs, ys1)
  }
}

def merge2[T](lessThan: (T, T) => Boolean)(xs: List[T], ys: List[T]): List[T] =
  (xs, ys) match {
    case (Nil, Nil) => Nil
    case (xs, Nil) => xs
    case (Nil, ys) => ys
    case (x :: xs1, y :: ys1) =>
      if (lessThan(x,y)) x :: merge2(lessThan)(xs1, ys)
      else y :: merge2(lessThan)(xs, ys1)
  }

def merge3[T](ord: Ordering[T])(xs: List[T], ys: List[T]): List[T] =
  (xs, ys) match {
    case (Nil, Nil) => Nil
    case (xs, Nil) => xs
    case (Nil, ys) => ys
    case (x :: xs1, y :: ys1) =>
      if (ord.lt(x, y)) x :: merge3(ord)(xs1, ys)
      else y :: merge3(ord)(xs, ys1)
  }

def msort2[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, Nil) => Nil
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val (fst, snd) = xs splitAt n
    merge(msort2(fst), msort2(snd))
  }
}

val nums = List(2, -4, 5, 7, 1)
val fruits = List("apple", "pineapple", "orange", "banana")

val ltInts = (x: Int, y: Int) => x < y
msort(nums)(merge(ltInts))
msort(nums)(merge2(ltInts))
msort(nums)(merge3(Ordering.Int))

val ltStrings = (x: String, y: String) => (x compareTo y) < 0
msort(fruits)(merge(ltStrings))
msort(fruits)(merge2(ltStrings))
msort(fruits)(merge3(Ordering.String))

msort2(nums)
msort2(fruits)