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

  def execute(program: Program, env: Environment): Environment = {
    val newEnv = env.setLine(number)
    statement.execute(program, newEnv)
  }

  def isNextFor(variable: VariableIndex): Boolean =
    statement.isInstanceOf[NEXT] && statement.asInstanceOf[NEXT].isNextFor(variable)
}

abstract class AssignmentBase(val variableIndex: VariableIndex, val expression: Expression) extends Listable {
  override def list: String = f"${variableIndex.list}=${expression.list}"
}

case class Assignment(override val variableIndex: VariableIndex, override val expression: Expression)
  extends AssignmentBase(variableIndex,expression)

object Assignment {
  def apply(variable: Variable, expression: Expression): Assignment = new Assignment(VariableIndex(variable,Index.empty), expression)
}

case class NumericAssignment(override val variableIndex: VariableIndex, numExpression: NumericExpression)
  extends AssignmentBase(variableIndex,numExpression) {
  override def list: String = f"${variableIndex.list}=${expression.list}"
}

object NumericAssignment {
  def apply(variable: Variable, expression: NumericExpression): NumericAssignment =
    new NumericAssignment(VariableIndex(variable,Index.empty), expression)
}

