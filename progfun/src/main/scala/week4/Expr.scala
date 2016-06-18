package week4

trait Expr {
  private def op(f: (Int, Int) => Int)(e1: Expr, e2: Expr): String =  e1 match {
    case Var(name) => name + e2.eval()
    case _ => e2 match {
      case Var(name) => e1.eval() + name
      case _ => f(e1.eval().toInt, e2.eval().toInt).toString
    }
  }

  private def format(op: String)(e1: Expr, e2: Expr): String =  s"${e1.show()} $op ${e2.show()}"

  def eval(): String = this match {
    case Num(n) => n.toString
    case Sum(x, y) => op(_+_)(x, y)
    case Var(name) => name
    case Prod(x, y) => op(_*_)(x, y)
  }

  def show(): String = this match {
    case Num(n) => n.toString
    case Sum(x, y) => format("+")(x, y)
    case Var(_) => this.eval()
    case Prod(x, y) => x match {
      case Sum(_,_) => s"(${x.show()}) * ${y.show()}"
      case _ => y match {
        case Sum(_,_) => s"${x.show()} * (${y.show()})"
        case _ => format("*")(x, y)
      }
    }
  }

  override def toString = show()
}

case class Num(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr