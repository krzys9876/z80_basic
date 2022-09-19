package org.kr.scala.z80.program.parser

import org.kr.scala.z80.expression.StaticTextExpr
import org.kr.scala.z80.program.{Assignment, AssignmentBase, FOR, GOTO, LET, Line, LineNumber, NEXT, NumericAssignment, PRINT, REM, Statement, Variable}

import scala.util.parsing.combinator.JavaTokenParsers

//TODO: change all string matchers to be case insensitive AND convert all variable names to upper case
abstract class BaseParser[T]() extends JavaTokenParsers {
  def result:Parser[T]

  def process(input: String): Either[String, T] = {
    parseAll(result, input) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess =>
        //scala.sys.error(failure.msg)
        Left(failure.msg)
    }
  }
}

case class LineParser() extends BaseParser[Line] with CommonParser with LineNumberParser with StatementParser {
  def result: Parser[Line] = lineNumber ~ statement ^^ { case l ~ s => Line(l, s) }
}

object LineParser {
  def apply(input: String): Either[String, Line] = LineParser().process(input)

  def force(input: String): Line = LineParser().process(input).toOption.get
}

trait CommonParser extends JavaTokenParsers {
  def integerNumber: Parser[String] = """(\d+)""".r
  def anyText: Parser[String] = """(.*)""".r
  def anyTextQuoted:Parser[String] = anyText ^^ {
    case t if t.startsWith("'") && t.endsWith("'") => removeFirstAndLastCharacter(t)
    case t if t.startsWith("\"") && t.endsWith("\"") => removeFirstAndLastCharacter(t)
    case t => t}
  def emptyString:Parser[String] = """(^$)""".r
  private def removeFirstAndLastCharacter(t:String):String = t.substring(1,t.length-1)
}

trait LineNumberParser extends CommonParser {
  def lineNumber:Parser[LineNumber] = integerNumber ^^ {num => LineNumber(num.toInt) }
}

trait StatementParser extends CommonParser with RemParser with PrintParser with ForParser with NextParser
  with LetParser with GotoParser {
  def statement: Parser[Statement] = rem | print | for_ | next | let | goto
}

trait RemParser extends CommonParser {
  def rem:Parser[REM] = "REM" ~ (anyTextQuoted | emptyString) ^^ {case _ ~ t => REM(t)}
}

trait StaticTextExprParser extends CommonParser {
  def staticTextExpr:Parser[StaticTextExpr] = (anyTextQuoted | emptyString) ^^ {t => StaticTextExpr(t)}
}

trait PrintParser extends CommonParser with StaticTextExprParser with NumericExpressionParser {
  def print:Parser[PRINT] = "PRINT" ~ (numericExpression | staticTextExpr) ^^ {case _ ~ t => PRINT(t)}
}

trait VariableParser extends CommonParser {
  def numVariable:Parser[Variable]=numVariableName ^^ {Variable(_)}
  def textVariable:Parser[Variable]=textVariableName ^^ {Variable(_)}
  private def numVariableName:Parser[String]="""([A-Z]+)""".r
  private def textVariableName:Parser[String]="""([A-Z]+$)""".r
}

trait NextParser extends CommonParser with VariableParser {
  def next:Parser[NEXT] =
    "NEXT" ~ numVariable ^^ {case _ ~ v => NEXT(Some(v))} |
      "NEXT" ~ emptyString ^^ {_ => NEXT()}
}

trait ForParser extends CommonParser with VariableParser with AssignmentParser {
  def for_ :Parser[FOR] =
    "FOR" ~ numericAssignment ~ "TO" ~ numericExpression ~ "STEP" ~numericExpression  ^^ {
      case _ ~ a ~ _ ~ to ~ _ ~ s => FOR(a,to,Some(s))} |
    "FOR" ~ numericAssignment ~ "TO" ~ numericExpression ^^ { case _ ~ a ~ _ ~ to => FOR(a,to)}
}

trait AssignmentParser extends VariableParser with NumericExpressionParser {
  def numericAssignment:Parser[NumericAssignment] = numVariable ~ "=" ~ numericExpression ^^ {
    case v ~ _ ~ e => NumericAssignment(v,e)
  }
  //TODO: extend text assignment with text expressions (after it is implemented)
  def textAssignment:Parser[Assignment] = textVariable ~ "=" ~ anyTextQuoted ^^ {
    case v ~ _ ~ e => Assignment(v,StaticTextExpr(e))
  }
}

trait LetParser extends AssignmentParser {
  def let:Parser[LET] = (numericAssignment | textAssignment) ^^ {a => LET(a)} |
    "LET" ~ (numericAssignment | textAssignment) ^^ {case _ ~ a => LET(a)}
}

trait GotoParser extends CommonParser {
  def goto:Parser[GOTO] = "GOTO" ~ integerNumber ^^ {case _ ~ l => GOTO(l.toInt)}
}