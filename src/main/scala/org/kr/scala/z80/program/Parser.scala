package org.kr.scala.z80.program

import org.kr.scala.z80.expression.StaticTextExpr

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

case class LineParser() extends CommonParser with LineNumberParser with StatementParser {
  def result: Parser[Line] = lineNumber ~ statement ^^ {case l ~ s => Line(l,s)}

  def process(input: String): Either[String, Line] = {
    parseAll(result, input) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess =>
        //scala.sys.error(failure.msg)
        Left(failure.msg)
    }
  }
}

object LineParser {
  def apply(input:String):Either[String,Line] = LineParser().process(input)
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

trait StatementParser extends CommonParser with RemParser with PrintParser {
  def statement: Parser[Statement] = rem | print
}

trait RemParser extends CommonParser {
  def rem:Parser[REM] = "REM" ~ (anyTextQuoted | emptyString) ^^ {case _ ~ t => REM(t)}
}

trait StaticTextExprParser extends CommonParser {
  def staticTextExpr:Parser[StaticTextExpr] = (anyTextQuoted | emptyString) ^^ {t => StaticTextExpr(t)}
}

trait PrintParser extends CommonParser with StaticTextExprParser {
  def print:Parser[PRINT] = "PRINT" ~ staticTextExpr ^^ {case _ ~ t => PRINT(t)}
}
