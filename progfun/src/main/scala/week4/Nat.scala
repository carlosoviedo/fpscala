package week4

import scala.annotation.tailrec

/**
  * TODO: include javadoc
  */
abstract class Nat {
  def isZero: scala.Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  private def throwException(method: String) =
    throw new RuntimeException(s"Zero.$method")
  def isZero = true
  def predecessor = throwException("predecessor")
  def + (that: Nat) = that
  def - (that: Nat) =
    if (that.isZero) this
    else throwException("-")

  override def toString = "0"
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) =
    if (that.isZero) this
    else n - that.predecessor

  override def toString = {
    @tailrec
    def loop(n: Nat, acc: Int): String =
      if (n.isZero) acc.toString
      else loop(n.predecessor, acc + 1)

    loop(this, 0)
  }
}