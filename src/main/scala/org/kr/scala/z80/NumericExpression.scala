package org.kr.scala.z80

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

  override def result:Any=numValue
  override def resultNum: Option[BigDecimal]=Some(numValue)
  override def resultText: Option[String]=Some(if(numValue.isWhole) intFormat.format(numValue) else numValue.toString)

  override def valueNum(env:Environment): Option[BigDecimal] = Some(numValue)
}

case class ExprVariable(variable: Variable) extends NumericExpression {
  override def evaluate(env:Environment): Either[String, BigDecimal] =
    env.getValueAs[BigDecimal](variable) match {
      case None => Left(f"missing value for variable: ${variable.name}")
      case Some(result) => Right(result)
    }

  override def result:Any=0
  override def resultNum: Option[BigDecimal]=None
  override def resultText: Option[String]=None

  override def valueNum(env:Environment): Option[BigDecimal] = evaluate(env).toOption
}

