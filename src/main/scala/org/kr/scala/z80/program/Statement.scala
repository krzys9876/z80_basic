package org.kr.scala.z80.program

import org.kr.scala.z80.environment.{Environment, ExitCode, ForState, ForStatus}
import org.kr.scala.z80.expression.{Expression, NumericExpression, TextExpression}

trait Statement extends Listable {
  override def list: String
  def execute(program: Program, environment: Environment): Environment
}

case class FOR(assignment: NumericAssignment, endValue: NumericExpression, step:Option[NumericExpression]) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = {
    environment.getFor(assignment.variable) match {
      case None =>
        val startVal=assignment.expression.valueNum(environment)
        val endVal=endValue.valueNum(environment)
        val stepVal=step.flatMap(_.valueNum(environment)).getOrElse(BigDecimal(1))
        (startVal,endVal) match {
          case (None,_) | (_,None) => environment.setExitCode(ExitCode.FATAL_FOR_MISSING_VALUE)
          case (Some(startV),Some(endV)) if startV>endV=>finishFor(program,environment)
          case (Some(startV),Some(endV))=>initFor(environment,startV,endV,stepVal)
        }
      case Some(state) =>
        calculateNextValue(environment,state.step) match {
          case Some(nextValue) if nextValue > state.end => finishFor(program, environment)
          case Some(nextValue) => continueFor(environment,nextValue)
          case None=>environment.setExitCode(ExitCode.FATAL_FOR_CANNOT_GET_VALUE)
        }
    }
  }

  private def calculateNextValue(environment: Environment,stepVal:BigDecimal):Option[BigDecimal] = {
    environment.getValueAs[BigDecimal](assignment.variable) match {
      case None=>None
      case Some(value)=> Some(value + stepVal)
    }
  }

  private def continueFor(environment: Environment, nextValue:BigDecimal):Environment =
    environment
      .setVariable(assignment.variable, nextValue)

  private def initFor(environment: Environment,start:BigDecimal,end:BigDecimal,step:BigDecimal):Environment =
      environment
        .setVariable(assignment.variable, assignment.expression.valueNum(environment).get)
        .setForStack(assignment.variable, environment.getCurrentLine.get,start,end,step)

  private def finishFor(program: Program,environment: Environment):Environment = {
    program.getNextFor(assignment.variable, environment.getCurrentLine.get) match {
      case Some(nextLine)=>environment
        .finishForStack(assignment.variable)
        .forceNextLine(nextLine) // end of loop
      case None => environment.setExitCode(ExitCode.MISSING_NEXT)
      }
    }

  override def list: String = f"FOR ${assignment.variable.name} = " +
    f"${assignment.expression.list} TO ${endValue.list}" +
    step.map(s=>f" STEP ${s.list}").getOrElse("")
}

object FOR {
  def apply(assignment: NumericAssignment, expression: NumericExpression, step:Option[NumericExpression]=None): FOR =
    new FOR(assignment,expression,step)
}

//TODO: replace variable with optional list of variables
// Empty list means that NEXT terminates the most recent FOR loop)
// Multiple variables are treated as consecutive NEXT statements
case class NEXT(variable: Option[Variable]=None) extends Statement {
  override def execute(program: Program, environment: Environment): Environment =
    environment.getFor(variable) match {
      case Some(ForState(forVariable,_,_,_,_, ForStatus.FINISHED)) => environment.clearForStack(forVariable)
      case Some(ForState(_,_,_,_, forLine, _)) => environment.forceNextLine(forLine)
      case None => environment.setExitCode(ExitCode.MISSING_FOR)
    }

  def isNextFor(checkVariable:Variable):Boolean =
    variable.isEmpty || variable.contains(checkVariable)

  override def list: String = f"NEXT"+variable.map(" "+_.name).getOrElse("")
}

object NEXT {
  def apply(variable: Variable): NEXT = new NEXT(Some(variable))
}

case class PRINT(expression: Expression) extends Statement {
  // print text to console
  override def execute(program: Program, environment: Environment): Environment = {
    //TODO: decode missing value properly (return exit code if variable cannot be decoded)
    environment.consolePrintln(expression.valueText(environment))
  }

  override def list: String = f"PRINT ${expression.list}"
}

case class REM(comment: String) extends Statement {
  // ignore the line
  override def execute(program: Program, environment: Environment): Environment = environment

  override def list: String = f"REM $comment"
}

case class LET(assignment: AssignmentBase) extends Statement {
  // assign a value to a variable
  override def execute(program: Program, environment: Environment): Environment = {
    assignment.expression match {
      case num : NumericExpression => environment.setVariable(assignment.variable, num.valueNum(environment).get)
      case text : TextExpression => environment.setVariable(assignment.variable, text.valueText(environment))
    }
  }

  override def list: String = f"LET ${assignment.variable.name} = ${assignment.expression.list}"
}
