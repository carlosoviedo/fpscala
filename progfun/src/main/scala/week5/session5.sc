import scala.annotation.tailrec

def reduceLeft[T](xs: List[T])(op: (T,T) => T): T = xs match {
  case Nil => throw new NoSuchElementException("Nil.reduceLeft")
  case x :: xs1 => foldLeft(xs1)(x)(op)
}

@tailrec
def foldLeft[T,U](xs: List[T])(z: U)(op: (U, T) => U): U = xs match {
  case Nil => z
  case x :: xs1 => foldLeft(xs1)(op(z, x))(op)
}

def reduceRight[T](xs: List[T])(op: (T,T) => T): T = xs match {
  case Nil => throw new NoSuchElementException("Nil.reduceRight")
  case x :: xs1 => foldRight(xs.init)(xs.last)(op)
}

def foldRight[T,U](xs: List[T])(z: U)(op: (T, U) => U): U = xs match {
  case Nil => z
  case x :: xs1 => op(x, foldRight(xs1)(z)(op))
}

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys)(_ :: _)

def mapFun[T,U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x, z) => z + 1)

val xs = List(2,5,3,7,8)
val ys = List(0, 1, 9)
reduceLeft(0 :: xs)(_+_)
foldLeft(xs)(0)(_+_)
reduceRight(0::xs)(_+_)
foldRight(xs)(0)(_+_)
concat(xs, ys)
mapFun(xs, (x: Int) => x / 2)
lengthFun(xs)
lengthFun(ys)