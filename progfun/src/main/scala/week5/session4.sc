def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case x :: xs1 => x*x :: squareList(xs1)
}

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x*x)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (equal, different) = xs span (_ == x)
    equal :: pack(different)
  }
}

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

val xs = List(1,5,4,7)
squareList(xs)
squareList2(xs)
val ys = List("a", "a", "a", "b", "c", "c", "a")
pack(ys)
encode(ys)