package org.kr.scala.z80.expression

import org.kr.scala.z80.environment.Environment

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
