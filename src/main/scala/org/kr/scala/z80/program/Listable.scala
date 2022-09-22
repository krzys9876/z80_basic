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

case class Line(number: LineNumber, statements: Vector[Statement]) extends Listable {
  override def list: String = f"$number ${statements.map(_.list).mkString(":")}"

  def execute(program: Program, env: Environment, id:StatementId, runFunction:Statement.processLineType): Environment =
    runFunction(statements(id.statementNum), program, env)

  def isNextFor(variable: Variable): Option[(Line,Int)] =
    statements.indexWhere(stmt=>stmt.isInstanceOf[NEXT] && stmt.asInstanceOf[NEXT].isNextFor(variable)) match {
      case -1=>None
      case i=>Some((this,i))
    }

  def statementCount:Int=statements.length
}


object Line {
  def apply(number:LineNumber, statement:Statement)=new Line(number,Vector(statement))
}

case class StatementId(lineNumber: LineNumber, statementNum: Int) {
  def isBefore(other:StatementId):Boolean = {
    lineNumber.num<other.lineNumber.num ||
    (lineNumber.num==other.lineNumber.num && statementNum<other.statementNum)
  }
  def nextSameLine:StatementId=copy(statementNum=statementNum+1)
}

object StatementId {
  def apply(lineNumber: LineNumber):StatementId = new StatementId(lineNumber,0)
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

