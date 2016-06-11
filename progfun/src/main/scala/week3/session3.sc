import week3.{List, Cons, Nil}

def nth[T](n: Int, list: List[T]): T = {
  def loop(counter: Int, xs: List[T]): T =
    if (xs.isEmpty) throw new IndexOutOfBoundsException(s"Index out of bounds: $n")
    else if (counter == n) xs.head
    else loop(counter+1, xs.tail)

  if (n < 0) throw new IndexOutOfBoundsException(s"Index out of bounds: $n")
  else loop(0, list)
}

def nth_coursera[T](n: Int, xs: List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException(s"Index out of bounds: $n")
  else if (n == 0) xs.head
  else nth_coursera(n - 1, xs.tail)

val x = new Cons(3, new Cons(4, new Cons(5, new Cons(6, Nil))))
println(nth(0, x))
println(nth(3, x))