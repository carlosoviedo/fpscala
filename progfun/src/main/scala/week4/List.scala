package week4

trait List[+T] {
  def isEmpty: scala.Boolean
  def head: T
  def tail: List[T]
  def prepend[S >: T](elem: S): List[S] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

  override def toString = s"[$head -> $tail]"
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Head of empty list")
  def tail = throw new NoSuchElementException("Tail of empty list")

  override def toString = "."
}

/**
  * TODO: include javadoc
  */
object List {
  def apply() = Nil
  def apply[T](x: T) = new Cons(x, Nil)
  def apply[T](x: T, y: T) = new Cons(x, new Cons(y, Nil))
}

object test {
  val x: List[String] = Nil
  def f(xs: List[String], x: Int) = xs prepend x
}
