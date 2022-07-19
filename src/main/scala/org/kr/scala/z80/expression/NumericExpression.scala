package org.kr.scala.z80.expression

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.program.Variable

import java.text.DecimalFormat

abstract class NumericExpression extends Expression {
  def evaluate(env:Environment): Either[String, BigDecimal]

  val intFormat=new DecimalFormat("#")

  override def valueText(env: Environment):String=
    valueNum(env) match {
      case None => ""
      case Some(number) => if(number.isWhole) intFormat.format(number) else number.toString
    }
}

case class ExprNumber(numValue: Double) extends NumericExpression {
  override def evaluate(env:Environment): Either[String, BigDecimal] = Right(numValue)
  override def toString: String = f"NUM:$numValue"

  override def valueNum(env:Environment): Option[BigDecimal] = Some(numValue)

  override def list: String = if(numValue.isWhole) intFormat.format(numValue) else numValue.toString
}

object ExprNumber {
  def apply(bool:Boolean):ExprNumber = new ExprNumber(if(bool) -1 else 0)
}

case class ExprVariable(variable: Variable) extends NumericExpression {
  override def evaluate(env:Environment): Either[String, BigDecimal] =
    env.getValueAs[BigDecimal](variable) match {
      case None => Left(f"missing value for variable: ${variable.name}")
      case Some(result) => Right(result)
    }

  override def valueNum(env:Environment): Option[BigDecimal] = evaluate(env).toOption

  override def list: String = variable.name
}

