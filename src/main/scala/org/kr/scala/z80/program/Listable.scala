package org.kr.scala.z80.program

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.expression.{Expression, NumericExpression}

import scala.language.implicitConversions

trait Listable {
  // 'list' is not a container but rather a verb 'to list', i.e. generate textual representation of an element of the program
  def list: String
}

case class LineNumber(num: Int, endOfProgram: Boolean = false) {
  override def toString: String = num.toString
}

object LineNumber {
  implicit def fromInt(num:Int):LineNumber = LineNumber(num)
}

case class Line(number: LineNumber, statement: Statement) extends Listable {
  override def list: String = f"$number ${statement.list}"

  def execute(program: Program, env: Environment, runFunction:Statement.processLineType): Environment =
    runFunction(statement, program, env.setLine(number))

  def isNextFor(variable: Variable): Boolean =
    statement.isInstanceOf[NEXT] && statement.asInstanceOf[NEXT].isNextFor(variable)
}

abstract class AssignmentBase(val variable: Variable, val expression: Expression) extends Listable {
  override def list: String = f"${variable.list}=${expression.list}"
}

case class Assignment(override val variable: Variable, override val expression: Expression)
  extends AssignmentBase(variable,expression)

object Assignment {
  def apply(variable: VariableName, expression: Expression): Assignment = new Assignment(Variable(variable,ExprIndex.empty), expression)
}

case class NumericAssignment(override val variable: Variable, numExpression: NumericExpression)
  extends AssignmentBase(variable,numExpression) {
  override def list: String = f"${variable.list}=${expression.list}"
}

object NumericAssignment {
  def apply(variable: VariableName, expression: NumericExpression): NumericAssignment =
    new NumericAssignment(Variable(variable,ExprIndex.empty), expression)
}

