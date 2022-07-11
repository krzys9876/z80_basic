package org.kr.scala.z80

trait Statement extends Listable {
  override def list: String
  def execute(program: Program, environment: Environment): Environment
}

class FOR(val assignment: Assignment, val endValue: Expression, val step:Option[Expression]) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = {
    val state=environment.getFor(assignment.variable)
    val startVal=assignment.expression.resultNum
    val endVal=endValue.resultNum
    (state,startVal,endVal) match {
      case (_,_,None) | (_,None,_) => environment.setExitCode(ExitCode.FATAL_FOR_MISSING_VALUE)
      case (None,Some(start),Some(end)) if start>end => finishFor(program,environment)
      case (None,Some(start),Some(end)) => initFor(environment)
      case (Some(_),Some(_),Some(endVal)) =>
        calculateNextValue(environment) match {
          case Some(nextValue) if nextValue > endVal => finishFor(program, environment)
          case Some(nextValue) => continueFor(environment,nextValue)
          case None=>environment.setExitCode(ExitCode.FATAL_FOR_CANNOT_GET_VALUE)
        }
    }
  }

  private def calculateNextValue(environment: Environment):Option[BigDecimal] = {
    environment.getValueAs[Result](assignment.variable) match {
      case None=>None
      case Some(value)=> value.resultNum.map(_ + stepNum)
    }
  }

  lazy val stepNum: BigDecimal =step.flatMap(_.resultNum).getOrElse(BigDecimal(1))

  private def continueFor(environment: Environment, nextValue:BigDecimal):Environment =
    environment
      .setVariable(assignment.variable, Result(nextValue))

  private def initFor(environment: Environment):Environment =
      environment
        .setVariable(assignment.variable, assignment.expression)
        .setForStack(assignment.variable, environment.getCurrentLine.get)

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
  def apply(assignment: Assignment, expression: Expression, step:Option[Expression]=None): FOR =
    new FOR(assignment,expression,step)
}

//TODO: replace variable with optional list of variables
// Empty list means that NEXT terminates the most recent FOR loop)
// Multiple variables are treated as consecutive NEXT statements
class NEXT(val variable: Option[Variable]) extends Statement {
  override def execute(program: Program, environment: Environment): Environment =
    environment.getFor(variable) match {
      case Some(ForState(forVariable ,_, ForStatus.FINISHED)) => environment.clearForStack(forVariable)
      case Some(ForState(_, forLine, _)) => environment.forceNextLine(forLine)
      case None => environment.setExitCode(ExitCode.MISSING_FOR)
    }

  def isNextFor(checkVariable:Variable):Boolean =
    variable.isEmpty || variable.contains(checkVariable)

  override def list: String = f"NEXT"+variable.map(" "+_.name).getOrElse("")
}

object NEXT {
  def apply(variable: Variable): NEXT = new NEXT(Some(variable))
  def apply(): NEXT = new NEXT(None)
}

class PRINT(val expression: Expression) extends Statement {
  // print text to console
  override def execute(program: Program, environment: Environment): Environment = {
    val output = expression.result.toString
    environment.consolePrintln(output)
  }

  override def list: String = f"PRINT \"${expression.list}\""
}

object PRINT {
  def apply(expression: Expression): PRINT = new PRINT(expression)
}

class REM(val comment: String) extends Statement {
  // ignore the line
  override def execute(program: Program, environment: Environment): Environment = environment

  override def list: String = f"REM $comment"
}

object REM {
  def apply(comment: String): REM = new REM(comment)
}

class LET(val assignment: Assignment) extends Statement {
  // print text to console
  override def execute(program: Program, environment: Environment): Environment = {
    environment.setVariable(assignment.variable, assignment.expression)
  }

  override def list: String = f"LET ${assignment.variable.name} = ${assignment.expression.list}"
}

object LET {
  def apply(assignment: Assignment): LET = new LET(assignment)
}

