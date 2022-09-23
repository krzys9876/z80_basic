package org.kr.scala.z80.parser

import org.kr.scala.z80.program.{Line, LineNumber}

import scala.util.parsing.combinator.JavaTokenParsers

//NOTE: all string matchers are case insensitive AND all variables and keywords should be upper case
abstract class BaseParser[T]() extends JavaTokenParsers {
  def result:Parser[T]

  def process(input: String): Either[String, T] = {
    parseAll(result, BaseParser.escapeBackslash(input)) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)
    }
  }
}

object BaseParser {
  val backslash="\\"
  val backslashEscaed="~~"
  def escapeBackslash(text:String):String=text.replace(backslash,backslashEscaed)
  def unEscapeBackslash(text:String):String=text.replace(backslashEscaed,backslash)
}

case class LineParser() extends BaseParser[Line] with CommonParser with LineNumberParser with StatementParser {
  def result: Parser[Line] = lineNumber ~ rep1sep(statement,":") ^^ { case l ~ s => Line(l, s.toVector) }
}

object LineParser {
  def apply(input: String): Either[String, Line] = LineParser().process(input)

  def force(input: String): Line = LineParser().process(input).toOption.get
}

trait LineNumberParser extends CommonParser {
  def lineNumber:Parser[LineNumber] = integerNumber ^^ {num => LineNumber(num.toInt) }
}

