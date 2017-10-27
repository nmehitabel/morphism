package io.mehitabel.morphism
package explicit

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
  // repetition of essentially the same process
  val mkString: Exp => String = {
    case IntValue(v) => v.toString
    case DecValue(v) => v.toString
    case Sum(e1, e2) => s"( ${mkString(e1)} + ${mkString(e2)} )"
    case Multiply(e1, e2) => s"( ${mkString(e1)} * ${mkString(e2)} )"
    case Divide(e1, e2) => s"( ${mkString(e1)} / ${mkString(e2)} )"
    case Square(e1) => s"( ${mkString(e1)}^2 )"
  }

  //And all over again but also optimize only helpful for one case but
  //nonetheless all must be accounted for
  val optimize: Exp => Exp = {
    case Multiply(e1, e2) if (e1 == e2) => Square(optimize(e1))
    case IntValue(v) => IntValue(v) // if optimize(v) then will compile but won't terminate!!!
    case DecValue(v) => DecValue(v)
    case Sum(e1, e2) => Sum(optimize(e1), optimize(e2))
    case Multiply(e1, e2) => Multiply(optimize(e1), optimize(e2))
    case Square(e) => Square(optimize(e))
    case Divide(e1, e2) => Divide(optimize(e1), optimize(e2))
  }
}

object Example {
  val exp2: Exp =
    Divide(
      DecValue(5.2),
      Sum(
        IntValue(10),
        IntValue(5)
      )
    )
}
