package org.kr.scala.z80

abstract class Expression extends Listable {
  def result: Any
  def resultNum: Option[BigDecimal]
  def resultText: Option[String]

  def valueNum(env:Environment): Option[BigDecimal]
  def valueText(env: Environment):String = resultText.getOrElse("")
}

