package io.mehitabel.morphism
package exp

sealed trait Exp
final case class IntValue(v: Int) extends Exp
final case class DecValue(v: Double) extends Exp
final case class Sum(exp1: Exp, exp2: Exp) extends Exp
final case class Multiply(exp1: Exp, exp2: Exp) extends Exp
final case class Divide(exp1: Exp, exp2: Exp) extends Exp
final case class Square(exp: Exp) extends Exp

// mechanical process
object Evaluater {
  val evaluate: Exp => Double = {
    case IntValue(v) => v.toDouble
    case DecValue(v) => v
    case Sum(e1, e2) => evaluate(e1) + evaluate(e2)
    case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)
    case Divide(e1, e2) => evaluate(e1) / evaluate(e2)
    case Square(e1) =>
      val v = evaluate(e1)
      v * v
  }

  val mkString: Exp => String = {
    case IntValue(v) => v.toString
    case DecValue(v) => v.toString
    case Sum(e1, e2) => s"( ${mkString(e1)} + ${mkString(e2)} )"
    case Multiply(e1, e2) => s"( ${mkString(e1)} * ${mkString(e2)} )"
    case Divide(e1, e2) => s"( ${mkString(e1)} / ${mkString(e2)} )"
    case Square(e1) => s"( ${mkString(e1)}^2 )"
  }
}
