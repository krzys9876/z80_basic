package org.kr.scala.z80.expression

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.program.Listable

abstract class Expression extends Listable {
  def valueNum(env:Environment): Option[BigDecimal]
  def valueText(env: Environment):String
}

