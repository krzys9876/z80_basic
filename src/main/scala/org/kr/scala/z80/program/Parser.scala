package org.kr.scala.z80.program

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

abstract class BaseParser[T] extends JavaTokenParsers {
  def result: Parser[T]

  def process(input: String): Either[String, T] = {
    parseAll(result, input) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess =>
        //scala.sys.error(failure.msg)
        Left(failure.msg)
    }
  }

  def anyText: Parser[String] = """(.*)""".r
}

case class LineParser() extends BaseParser[Line] {
  def result:Parser[Line] = lineNumber ~ statement ^^ {case l ~ s => Line(l,s)}

  private def lineNumber:Parser[LineNumber] = """(\d+)?""".r ^^ {num => LineNumber(num.toInt) }
  private def statement:Parser[Statement] = anyText ^^ {t=>StatementParser(t).toOption.get}
}

object LineParser {
  def apply(input:String):Either[String,Line] = LineParser().process(input)
}

case class StatementParser() extends BaseParser[Statement] {
  def result:Parser[Statement] = statement

  private def statement:Parser[Statement] =
    rem ~ anyText ^^ {case _ ~ text => RemParser(text).toOption.get}

  def rem:Parser[String]="REM"
}

object StatementParser {
  def apply(input:String):Either[String,Statement] = StatementParser().process(input)
}

case class RemParser() extends BaseParser[REM] {
  def result:Parser[REM] = (anyTextQuoted | emptyString) ^^ {t => REM(t)}

  private def anyTextQuoted:Parser[String] = anyText ^^ {
    case t if t.startsWith("'") && t.endsWith("'") => removeFirstAndLastCharacter(t)
    case t if t.startsWith("\"") && t.endsWith("\"") => removeFirstAndLastCharacter(t)
    case t => t}
  private def emptyString:Parser[String] = """(^$)""".r

  private def removeFirstAndLastCharacter(t:String):String = t.substring(1,t.length-1)
}

object RemParser {
  def apply(input:String):Either[String,REM] = RemParser().process(input)
}