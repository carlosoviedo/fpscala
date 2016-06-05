import scala.annotation.tailrec

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator cannot be 0")

  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)

  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  def this(x: Int) = this(x, 1)

  def +(that: Rational) =
    new Rational(
      this.numer * that.denom + this.denom * that.numer,
      this.denom * that.denom
    )

  def unary_- = new Rational(-numer, denom)

  def -(that: Rational) = this + -that

  def <(that: Rational) =
    this.numer * that.denom < this.denom * that.numer

  def max(that: Rational) =
    if (this < that) that
    else this

  override def toString = s"$numer/$denom"
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x - y - z
y + y
x < y
y < y
x max y
//val invalid = new Rational(4, 0)
new Rational(2)