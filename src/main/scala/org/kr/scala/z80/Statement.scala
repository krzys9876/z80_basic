package org.kr.scala.z80

trait Statement extends Listable {
  override def list: String
  def execute(program: Program, environment: Environment): Environment
}

class FOR(val assignment: Assignment, val endValue: Expression, val step:Option[Expression]) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = {
    val argNextStmt = program
      .getNextFor(assignment.variable, environment.getCurrentLine.get)

    val lineFor = environment.getFor(Left(assignment.variable.name))
    if (lineFor.isEmpty) { // start of loop
      environment
        .setVariable(assignment.variable, assignment.expression)
        .setForStack(assignment.variable.name, environment.getCurrentLine.get)
    } else {
      val nextValueResult = environment.getValue(assignment.variable).get.asInstanceOf[Result]
      val stepNum=step.flatMap(_.resultNum).getOrElse(BigDecimal(1))
      val nextValue = nextValueResult.resultNum.get + stepNum

      if (nextValue > endValue.resultNum.get)
        environment.clearForStack(assignment.variable.name).setNextLine(argNextStmt.get) // end of loop
      else
        environment
          .setVariable(assignment.variable, Result(nextValue)) // TODO: check for empty
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
  override def execute(program: Program, environment: Environment): Environment = {
    environment
      .getFor(variable.map(v=>Left(v.name)).getOrElse(Right(environment.getCurrentLine.get)))
      .map(environment.setNextLine)
      .getOrElse(environment)
  }

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

