package week3

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
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