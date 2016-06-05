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

  def add(that: Rational) =
    new Rational(
      this.numer * that.denom + this.denom * that.numer,
      this.denom * that.denom
    )

  def neg() = new Rational(-numer, denom)

  def sub(that: Rational) = this add that.neg

  def less(that: Rational) =
    this.numer * that.denom < this.denom * that.numer

  def max(that: Rational) =
    if (this less that) that
    else this

  override def toString = s"$numer/$denom"
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x sub y sub z
y add y
x less y
y less y
x max y
//val invalid = new Rational(4, 0)
new Rational(2)

