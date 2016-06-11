abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(value: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet =
    if (x < value) new NonEmpty(value, left incl x, right)
    else if (x > value) new NonEmpty(value, left, right incl x)
    else this

  def contains(x: Int): Boolean =
    if (x < value) left contains x
    else if (x > value) right contains x
    else true

  def union(other: IntSet): IntSet =
    left union right union other incl value

  override def toString = s"{$left$value$right}"
}

val x = new NonEmpty(5, new NonEmpty(3, Empty, Empty), new NonEmpty(7, Empty, Empty))
val y = new NonEmpty(4, new NonEmpty(2, Empty, Empty), new NonEmpty(10, Empty, Empty))
x union y