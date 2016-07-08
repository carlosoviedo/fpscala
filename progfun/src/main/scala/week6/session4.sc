def showCapital(value: Option[String]): String = value match {
  case Some(capital) => capital
  case None => "missing data"
}

val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")
val capital2 = capitalOfCountry withDefaultValue "<unknown>"

showCapital(capitalOfCountry get "andorra")
showCapital(capitalOfCountry get "US")
capital2("andorra")

val fruit = List("apple", "pear", "orange", "pineapple")
fruit sortWith (_.length < _.length)
fruit.sorted
fruit groupBy (_.head)

class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))

  private def adjust(entry: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = entry
    exp -> (coeff + terms(exp))
  }

  def ++ (other: Poly) =
    new Poly((other.terms foldLeft terms)(addTerm))

  private def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
    val (expr, coeff) = term
    terms + (expr -> (coeff + terms(expr)))
  }

  override def toString =
    (for ((expr, coeff) <- terms.toList.sorted.reverse) yield s"${coeff}x^$expr") mkString " + "
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2
p1 ++ p2