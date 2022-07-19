package org.kr.scala.z80

abstract class TextExpression extends Expression {
  def evaluate(env:Environment): Either[String, String]

  override def valueNum(env: Environment):Option[BigDecimal]=None
}

case class StaticTextExpr(text:String) extends TextExpression {
  override def evaluate(env:Environment): Either[String, String]=Right(text)
  override def toString: String = f"TEXT:$text"

  override def result:Any=text
  override def resultNum: Option[BigDecimal]=None
  override def resultText: Option[String]=Some(text)

  override def valueText(env:Environment): String = text

  override def list: String = f"\"$text\""
}