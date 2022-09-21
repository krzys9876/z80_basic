package org.kr.scala.z80.parser

import org.kr.scala.z80.expression.{BlankTextExpr, Expression, StaticTextExpr}
import org.kr.scala.z80.program.{Assignment, FOR, GOSUB, GOTO, IF, LET, Line, LineNumber, NEXT, NumericAssignment, PRINT, PrintableToken, REM, RETURN, Statement, Variable, VariableIndex}

import scala.util.parsing.combinator.JavaTokenParsers

//TODO: change all string matchers to be case insensitive AND convert all variable names to upper case
abstract class BaseParser[T]() extends JavaTokenParsers {
  def result:Parser[T]

  def process(input: String): Either[String, T] = {
    parseAll(result, input) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)
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

trait LineNumberParser extends CommonParser {
  def lineNumber:Parser[LineNumber] = integerNumber ^^ {num => LineNumber(num.toInt) }
}

