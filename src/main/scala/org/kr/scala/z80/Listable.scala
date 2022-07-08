package org.kr.scala.z80

trait Listable {
  // list not as a container but as 'listing', textual representation of an element of the program
  def list: String
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

abstract class Expression extends Listable {
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

class Assignment(val variable: Variable, val expression: Expression) extends Listable {
  override def list: String = f"${variable.list}=${expression.list}"
}

object Assignment {
  def apply(variable: Variable, expression: Expression): Assignment = new Assignment(variable, expression)
}

case class Variable(name: String) extends Listable {
  override def list: String = name
}

object Variable {
  def apply(name: String): Variable = new Variable(name)
}
