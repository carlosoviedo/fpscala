val xs = Array(1, 2, 3, 44)
xs map (_*2)

val s = "Hello World"
s filter (_.isUpper)

val r = 1 to 5
val y = 1 until 5
val z = 1 to 10 by 2
val a = 6 to 1 by -2

s exists (_.isUpper)
s forall (_.isUpper)

val pairs = List(1,2,3) zip s
pairs.unzip

s flatMap (List('.',_))
xs.sum
xs.product
xs.max
xs.min

(1 to 5) flatMap (x => (6 to 10) map (y => (x,y)))

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(pair => pair._1 * pair._2).sum

def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{case(x,y) => x*y}.sum

scalarProduct(Vector(1,2), Vector(3,4))
scalarProduct2(Vector(1,2), Vector(3,4))

def isPrime(n: Int): Boolean =
  (2 until n).forall(n % _ != 0)

isPrime(2)
isPrime(7)
isPrime(14)
