package org.kr.scala.z80.program

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.expression.{Expression, NumericExpression}

trait Listable {
  // 'list' is not a container but rather a verb 'to list', i.e. generate textual representation of an element of the program
  def list: String
}

case class LineNumber(num: Int, endOfProgram: Boolean = false) {
  override def toString: String = num.toString
}

case class Line(number: LineNumber, statement: Statement) extends Listable {
  override def list: String = f"$number ${statement.list}"

  def execute(program: Program, env: Environment): Environment = {
    val newEnv = env.setLine(number)
    statement.execute(program, newEnv)
  }

  def isNextFor(variable: Variable): Boolean =
    statement.isInstanceOf[NEXT] && statement.asInstanceOf[NEXT].isNextFor(variable)
}

class Assignment(val variable: Variable, val expression: Expression) extends Listable {
  override def list: String = f"${variable.list}=${expression.list}"
}

object Assignment {
  def apply(variable: Variable, expression: Expression): Assignment = new Assignment(variable, expression)
}

class NumericAssignment(val variable: Variable, val expression: NumericExpression) extends Listable {
  override def list: String = f"${variable.list}=${expression.list}"
}

object NumericAssignment {
  def apply(variable: Variable, expression: NumericExpression): NumericAssignment = new NumericAssignment(variable, expression)
}

case class Variable(name: String) extends Listable {
  override def list: String = name
}

object Variable {
  def apply(name: String): Variable = new Variable(name)
}
