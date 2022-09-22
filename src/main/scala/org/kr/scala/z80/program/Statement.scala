package org.kr.scala.z80.program

import org.kr.scala.z80.environment.{Environment, ExitCode, ForState, ForStatus}
import org.kr.scala.z80.expression.{BlankTextExpr, Expression, NumericExpression, TextExpression}

import scala.math.BigDecimal
import scala.util.Try

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
          case None=>environment.setExitCode(ExitCode.FATAL_CANNOT_GET_VALUE)
        }
    }
  }
  private def calculateNextValue(environment: Environment,stepVal:BigDecimal):Option[BigDecimal] = {
    environment.getValueAs[BigDecimal](assignment.variable) match {
      case Left(_)=>None
      case Right(value)=> Some(value + stepVal)
    }
  }
  private def continueFor(environment: Environment, nextValue:BigDecimal):Environment =
    environment.continueFor(assignment.variable, nextValue)

  private def initFor(environment: Environment,start:BigDecimal,end:BigDecimal,step:BigDecimal):Environment =
    assignment.expression.valueNum(environment) match {
      case None=>environment.setExitCode(ExitCode.FATAL_CANNOT_GET_VALUE)
      case Some(value)=>environment.initFor(assignment.variable,value,start,end,step)
    }

  private def finishFor(program: Program,environment: Environment):Environment = environment.finishFor(program,assignment.variable)
  override def list: String = f"FOR ${assignment.variable.name.name} = " +
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
  override def list: String = f"NEXT"+variable.map(" "+_.list).getOrElse("")
}

object NEXT {
  def apply(variable: Variable): NEXT = new NEXT(Some(variable))
}

case class PRINT(tokens: Vector[PrintableToken]) extends Statement {
  // print text to console
  override def execute(program: Program, environment: Environment): Environment =
    //TODO: decode missing value properly (return exit code if variable cannot be decoded)
    environment.consolePrint(tokens.map(_.printableText(environment)).mkString("")+endOfLineOrNone)
  private def lastToken:Option[PrintableToken] = Try(tokens.last).toOption
  private def shouldSkipEol:Boolean=lastToken.exists(_.isEmptySeparator)
  private def endOfLineOrNone:String=if(shouldSkipEol) "" else "\n"
  override def list: String = f"PRINT ${tokens.map(_.list).mkString("")}"
}

object PRINT {
  def apply(expression:Expression):PRINT=new PRINT(Vector(PrintableToken(None,expression)))
  def apply():PRINT=new PRINT(Vector())
}

case class PrintableToken(prefixSeparator:Option[String],expression: Expression) extends Listable {
  def printableText(environment: Environment):String = prefixToText+expression.valueText(environment)
  private def prefixToText:String=
    prefixSeparator match {
      //TODO: handle proper tabulation - add tab sign and handle it via console object (to be implemented)
      case Some(",")=>"\t"
      case _ => ""
    }
  def isEmptySeparator:Boolean =
    expression==BlankTextExpr && (prefixSeparator.contains(";") || prefixSeparator.contains(","))
  override def list: String = prefixSeparator.getOrElse("")+expression.list
}

object PrintableToken {
  def apply(expression: Expression):PrintableToken = new PrintableToken(None,expression)
}

case class REM(comment: String) extends Statement {
  // ignore the line
  override def execute(program: Program, environment: Environment): Environment = environment

  override def list: String = f"REM $comment"
}

case class LET(assignment: AssignmentBase) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = {
    assignment.expression match {
      case num : NumericExpression =>
        num.valueNum(environment) match {
          case None=>environment.setExitCode(ExitCode.FATAL_CANNOT_GET_VALUE)
          case Some(value)=>environment.setValue(assignment.variable, value)
        }
      case text : TextExpression => environment.setValue(assignment.variable, text.valueText(environment))
    }
  }
  override def list: String = f"LET ${assignment.variable.list} = ${assignment.expression.list}"
}

case class GOTO(toLine:LineNumber) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = {
    program.lineByNum(toLine) match {
      case Right(line) => environment.forceNextLine(line.number)
      case Left(code) => environment.setExitCode(code)
    }
  }
  override def list: String = f"GOTO ${toLine.num}"
}

case class IF(condition:NumericExpression,statement: Statement) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = {
    condition.valueNum(environment) match {
      case Some(v) if v==BigDecimal(0) => environment
      case Some(_) => statement.execute(program,environment)
      case None => environment.setExitCode(ExitCode.FATAL_IF_INVALID_CONDITION)
    }
  }
  override def list: String = f"IF ${condition.list} THEN ${statement.list}"
}

case class GOSUB(toLine:LineNumber) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = {
    program.lineByNum(toLine) match {
      case Right(_) => environment.pushLine(toLine)
      case Left(code) => environment.setExitCode(code)
    }
  }
  override def list: String = f"GOSUB ${toLine.num}"
}

case class RETURN() extends Statement {
  override def execute(program: Program, environment: Environment): Environment = environment.popLine(program)
  override def list: String = f"RETURN"
}

case class DIM(variableStatic: VariableStatic) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = environment.setArrayDim(variableStatic)
  override def list: String = f"DIM ${variableStatic.list}"
}

case class DATA(values:List[Any]) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = environment.storeData(values)
  override def list: String = f"DATA ${values.mkString(",")}"
}