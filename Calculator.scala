package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map { case (k, v) => (k, Signal(eval(v(), namedExpressions))) }
    
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v: Double) => v
      case Ref(name: String) => eval(getReferenceExpr(name, references), references.filter(p => p._1 != name))
      case Plus(a: Expr, b: Expr)   => eval(a, references) + eval(b, references)
      case Minus(a: Expr, b: Expr)  => eval(a, references) - eval(b, references)
      case Times(a: Expr, b: Expr)  => eval(a, references) * eval(b, references)
      case Divide(a: Expr, b: Expr) => b match {
        case Literal(v: Double) if v == 0.0 => Double.NaN
        case _ => eval(a, references) / eval(b, references)
      }        
      case _ => Double.NaN
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {            // Scala way of checking existence of variable
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
