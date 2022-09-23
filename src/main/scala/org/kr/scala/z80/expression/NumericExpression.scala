package org.kr.scala.z80.expression

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.program.Variable

import java.text.DecimalFormat
import scala.language.implicitConversions
import scala.util.Try

abstract class NumericExpression extends Expression {
  def evaluate(env:Environment): Either[String, BigDecimal]
  val intFormat=new DecimalFormat("#")
  override def valueText(env: Environment):String=
    valueNum(env) match {
      case None => ""
      case Some(number) => f" ${if(number.isWhole) intFormat.format(number) else number.toString}"
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
  implicit def fromDouble(num:Double):ExprNumber = new ExprNumber(num)
}

case class ExprVariable(variable: Variable) extends NumericExpression {
  override def evaluate(env:Environment): Either[String, BigDecimal] =
    env.getValueAs[BigDecimal](variable) match {
      case Left(_) => Left(f"missing value for variable: ${variable.list}")
      case Right(value) => Right(value)
    }
  override def valueNum(env:Environment): Option[BigDecimal] = evaluate(env).toOption
  override def list: String = variable.list
}

object ExprVariable {
  def apply(varName:String):ExprVariable=new ExprVariable(Variable.fromString(varName))
  implicit def fromString(varName:String):ExprVariable=new ExprVariable(Variable.fromString(varName))
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
      case (Left(msg1), Left(msg2)) => Left(f"$msg1, $msg2")
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
  override def list: String = f"(${factor1.list} $operator ${factor2.list})"
}

object ExprOperation {
  def plus(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"+")
  def minus(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"-")
  def mul(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"*")
  def div(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"/")
  def pow(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"^")
  def eq(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"=")
  def ne(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"<>")
  def gt(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,">")
  def lt(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"<")
  def ge(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,">=")
  def le(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"<=")
  def or(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"OR")
  def and(factor1:NumericExpression,factor2:NumericExpression):ExprOperation=ExprOperation(factor1,factor2,"AND")
}

case class ExprFunction(factor:NumericExpression, function:String) extends NumericExpression {
  override def evaluate(env: Environment): Either[String, BigDecimal] =
    factor.evaluate(env) match {
      case Right(v) =>
        function match {
          case "ABS" => Right(scala.math.abs(v.toDouble))
          case "SIN" => Right(scala.math.sin(v.toDouble))
          case "COS" => Right(scala.math.cos(v.toDouble))
          case "NOT" => bitwiseOper(v.toDouble, ~_)
          case "INT" => Right(scala.math.floor(v.toDouble))
          case "RND" => Right(scala.math.random())
          case "SQR" => Try(Right(BigDecimal(scala.math.sqrt(v.toDouble)))).getOrElse(Left("squaring error"))
          case "-" => Right(-v)
          case fun => Left(f"function: $fun is not supported")
        }
      case Left(msg) => Left(msg)
    }

  private def bitwiseOper(v:Double,func:Short=>Double):Either[String,BigDecimal] =
    if(v>32767 || v< -32768)
      Left("value out of range")
    else
      Right(func(v.toShort))

  override def valueNum(env:Environment): Option[BigDecimal] = evaluate(env).toOption
  override def list: String = f"$function(${factor.list})"
}

object ExprFunction {
  def abs(factor:NumericExpression):ExprFunction=ExprFunction(factor,"ABS")
  def sin(factor:NumericExpression):ExprFunction=ExprFunction(factor,"SIN")
  def cos(factor:NumericExpression):ExprFunction=ExprFunction(factor,"COS")
  def not(factor:NumericExpression):ExprFunction=ExprFunction(factor,"NOT")
  def neg(factor:NumericExpression):ExprFunction=ExprFunction(factor,"-")
  def int(factor:NumericExpression):ExprFunction=ExprFunction(factor,"INT")
  def sqr(factor:NumericExpression):ExprFunction=ExprFunction(factor,"SQR")
}