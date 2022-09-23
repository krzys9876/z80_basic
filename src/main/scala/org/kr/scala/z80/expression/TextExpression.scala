package org.kr.scala.z80.expression

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.program.Variable

import scala.language.implicitConversions

abstract class TextExpression extends Expression {
  def evaluate(env:Environment): Either[String, String]

  override def valueNum(env: Environment):Option[BigDecimal]=None
}

case class StaticTextExpr(text:String) extends TextExpression {
  override def evaluate(env:Environment): Either[String, String]=Right(text)
  override def toString: String = f"TEXT:$text"

  override def valueText(env:Environment): String = text

  override def list: String = f"\"$text\""
}

object BlankTextExpr extends TextExpression {
  override def evaluate(env:Environment): Either[String, String]=Right("")
  override def toString: String = f"(blank)"
  override def valueText(env:Environment): String = ""
  override def list: String = f"\"\""
}

case class TextExprVariable(variable: Variable) extends TextExpression {
  override def evaluate(env:Environment): Either[String, String] =
    env.getValueAs[String](variable) match {
      case Left(_) => Left(f"missing value for variable: ${variable.list}")
      case Right(value) => Right(value)
    }
  override def valueText(env:Environment): String = evaluate(env).toOption.getOrElse("")
  override def list: String = variable.list
}

object TextExprVariable {
  def apply(varName:String):ExprVariable=new ExprVariable(Variable.fromString(varName))
  implicit def fromString(varName:String):ExprVariable=new ExprVariable(Variable.fromString(varName))
}
