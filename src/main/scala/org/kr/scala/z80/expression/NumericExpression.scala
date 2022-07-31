package org.kr.scala.z80.expression

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.program.Variable

import java.text.DecimalFormat
import scala.util.Try

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

case class ExprOperation(factor1: NumericExpression, factor2: NumericExpression, operator: String) extends NumericExpression {
  override def evaluate(env: Environment): Either[String, BigDecimal] =
    (factor1.evaluate(env), factor2.evaluate(env)) match {
      case (Right(v1), Right(v2)) =>
        operator match {
          case "+" => Right(v1 + v2)
          case "-" => Right(v1 - v2)
          case "*" => Right(v1 * v2)
          case "/" => Try(Right(v1 / v2)).getOrElse(Left("division error"))
          case "^" => Right(scala.math.pow(v1.toDouble, v2.toDouble))
          case "=" => Right(boolToNum(v1==v2))
          case "<>" => Right(boolToNum(v1!=v2))
          case ">" => Right(boolToNum(v1>v2))
          case "<" => Right(boolToNum(v1<v2))
          case ">=" => Right(boolToNum(v1>=v2))
          case "<=" => Right(boolToNum(v1<=v2))
          case "OR" => bitwiseOper(v1,v2, _ | _)
          case "AND" => bitwiseOper(v1,v2, _ & _)
          case op => Left(f"operator: $op is not supported")
        }
      case (Right(_), Left(msg)) => Left(msg)
      case (Left(msg), Right(_)) => Left(msg)
    }

  private def bitwiseOper(v1:BigDecimal,v2:BigDecimal,func:(Short,Short)=>BigDecimal):Either[String,BigDecimal] =
    if(v1>32767 || v1< -32768 || v2>32767 || v2< -32768)
      Left("value out of range")
    else {
      // truncate result to short (16-bit)
      Right(func(v1.toShort,v2.toShort))
    }

  // -1 (not 1) represents True according to MS Basic documentation
  // it is a binary number consisting of only '1'
  private def boolToNum(bool:Boolean):Int=if(bool) -1 else 0

  override def valueNum(env:Environment): Option[BigDecimal] = evaluate(env).toOption

  override def list: String =
    f"(${factor1.list} $operator ${factor2.list})"
}
