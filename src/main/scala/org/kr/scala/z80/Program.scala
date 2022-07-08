package org.kr.scala.z80

class Program(val lines: Vector[Line]) {
  def show(): Unit = lines.foreach(line => println(line.list))

  def firstLineNumber: Option[LineNumber] =
    if (lines.isEmpty) None
    else Some(lines(0).number)

  def lineAfter(line: Line): Option[Line] = {
    val index = lines.indexOf(line)
    index match {
      case i if i < 0 => None // TODO: Throw exception???
      case i if i == lines.length - 1 => None // end of program
      case i => Some(lines(i + 1))
    }
  }

  def lineNumAfter(line: Line): Option[LineNumber] =
    lineAfter(line).map(_.number)
      .orElse(Some(LineNumber(Int.MaxValue, endOfProgram = true)))

  def line(lineNum: LineNumber): Option[Line] = lines.find(_.number == lineNum)

  def getNextFor(variable: Variable, from: LineNumber): Option[LineNumber] = {
    val forLine = lines.find(_.number == from)
    val forLineIndex = forLine.map(lines.indexOf).getOrElse(-1)
    if (forLineIndex >= 0) {
      lines
        .slice(forLineIndex, lines.length)
        .find(_.isNextFor(variable))
        .flatMap(lineNumAfter)
    }
    else None
  }
}

trait Listable {
  def list: String

  def listName: String = this.getClass.getSimpleName
}

case class LineNumber(num: Int, endOfProgram: Boolean = false) {
  override def toString: String = num.toString
}

class Line(val number: LineNumber, val statement: Statement) extends Listable {
  override def list: String = f"$number ${statement.list}"

  def execute(program: Program, env: Environment): Environment = {
    val newEnv = env.setLine(number)
    statement.execute(program, newEnv)
  }

  def isNextFor(variable: Variable): Boolean = {
    statement.isInstanceOf[NEXT] &&
      statement.asInstanceOf[NEXT].variable == variable
  }
}

trait Statement extends Listable {
  override def list: String = listName

  def execute(program: Program, environment: Environment): Environment
}

trait Token extends Listable

class FOR(val assignment: Assignment, val endValue: Expression, val step:Option[Expression]) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = {
    val argNextStmt = program
      .getNextFor(assignment.variable, environment.getCurrentLine.get)

    val lineFor = environment.getFor(assignment.variable.name)
    if (lineFor.isEmpty) { // start of loop
      environment
        .setVariable(assignment.variable, assignment.expression)
        .setForStack(assignment.variable.name, environment.getCurrentLine.get)
    } else {
      val nextValueResult = environment.getValue(assignment.variable).get.asInstanceOf[Result]
      val stepNum=step.flatMap(_.resultNum).getOrElse(BigDecimal(1))
      val nextValue = nextValueResult.resultNum.get + stepNum

      if (nextValue > endValue.resultNum.get)
        environment.setNextLine(argNextStmt.get) // end of loop
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

class NEXT(val variable: Variable) extends Statement {
  override def execute(program: Program, environment: Environment): Environment = {
    environment
      .getFor(variable.name)
      .map(environment.setNextLine)
      .getOrElse(environment)
  }

  override def list: String = f"NEXT ${variable.name}"
}

object NEXT {
  def apply(variable: Variable): NEXT = new NEXT(variable)
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

trait Keyword extends Token {
  override def list: String = listName
}

abstract class Expression extends Token {
  override def list: String =
    result match {
      case s: String => s
      case n: BigDecimal => n.toString()
      case n: Int => n.toString
      case n: Long => n.toString
      case n: Double => n.toString
      case b: Boolean => b.toString
      case _ => "TYPE NOT SUPPORTED"
    }

  val result: Any
  val resultNum: Option[BigDecimal]
  val resultText: Option[String]
}

case class Result(value: Any) extends Expression {
  override val result: Any = value
  override val resultNum: Option[BigDecimal] =
    value match {
      case n: BigDecimal => Some(n)
      case _ => None
    }
  override val resultText: Option[String] =
    value match {
      case s: String => Some(s)
      case _ => None
    }

  override def list: String = resultText.getOrElse(resultNum.map(_.toString).getOrElse("EMPTY"))
}

object Result {
  def apply(value: Any): Result = {
    val valueTyped = value match {
      case n: Int => BigDecimal(n)
      case n: Long => BigDecimal(n)
      case n: Double => BigDecimal(n)
      case n: BigDecimal => n
      case b: Boolean => BigDecimal(if (b) 1 else 0)
      case s: String => s
    }
    new Result(valueTyped)
  }
}

class Assignment(val variable: Variable, val expression: Expression) extends Token {
  override def list: String = f"${variable.list}=${expression.list}"
}

object Assignment {
  def apply(variable: Variable, expression: Expression): Assignment = new Assignment(variable, expression)
}

case class Variable(name: String) extends Token {
  override def list: String = name
}

object Variable {
  def apply(name: String): Variable = new Variable(name)
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

