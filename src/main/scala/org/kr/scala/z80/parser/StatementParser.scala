package org.kr.scala.z80.parser

import org.kr.scala.z80.expression.{BlankTextExpr, Expression, StaticTextExpr}
import org.kr.scala.z80.program.{Assignment, FOR, GOSUB, GOTO, IF, LET, NEXT, NumericAssignment, PRINT, PrintableToken, REM, RETURN, Statement, VariableIndex}

import scala.util.parsing.combinator.JavaTokenParsers

trait StatementParser extends CommonParser with StatementWithoutIfParser with IfParser {
  def statement: Parser[Statement] = statementWithoutIf | ifS
}

trait CommonParser extends JavaTokenParsers {
  def integerNumber: Parser[String] = """(\d+)""".r
  def anyText: Parser[String] = """(.*)""".r
  def anyTextQuoted:Parser[String] = stringLiteral ^^ stripQuotes
  def emptyString:Parser[String] = """(^$)""".r

  private def stripFirstAndLastChar(t:String):String = t.substring(1,t.length-1)
  private def stripQuotes(t:String):String =
    if(t.startsWith("\"") && t.endsWith("\"")) stripFirstAndLastChar(t) else t
}

// avoid circular inheritance (IfParser cannot inherit from StatementParser, which must inherit from IfParser)
trait StatementWithoutIfParser extends CommonParser with RemParser with PrintParser with ForParser with NextParser
  with LetParser with GotoParser with GosubParser with ReturnParser {
  def statementWithoutIf: Parser[Statement] = rem | print | for_ | next | let | goto | gosub | return_
}

trait RemParser extends CommonParser {
  def rem:Parser[REM] = "REM" ~ (anyText | emptyString) ^^ {case _ ~ t => REM(t)}
}

trait PrintParser extends CommonParser with NumericExpressionParser {
  def print:Parser[PRINT] = "PRINT" ~ opt(tokens) ^^ {case _ ~ t => PRINT(t.getOrElse(List()).toVector)}

  private def staticTextExpr:Parser[StaticTextExpr] = anyTextQuoted ^^ StaticTextExpr
  private def token:Parser[Expression]=numericExpression | staticTextExpr
  private def separator:Parser[String]=";" | ","
  private def tokenSep:Parser[PrintableToken]=separator ~ token ^^ {case s ~ t => PrintableToken(Some(s),t)}
  private def tokens:Parser[List[PrintableToken]]=
    token ~ opt(rep(tokenSep)) ~ opt(rep(separator)) ^^ {case t ~ l ~ s =>
      List(PrintableToken(None,t)) ++
        l.getOrElse(List()) ++
        s.getOrElse(List()).map(sep=>PrintableToken(Some(sep),BlankTextExpr))}
}

trait VariableParser extends CommonParser {
  def numVariable:Parser[VariableIndex]=numVariableName ^^ {VariableIndex.fromString}
  def textVariable:Parser[VariableIndex]=textVariableName ^^ {VariableIndex.fromString}
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
  def let:Parser[LET] = (numericAssignment | textAssignment) ^^ {LET(_)} |
    "LET" ~ (numericAssignment | textAssignment) ^^ {case _ ~ a => LET(a)}
}

trait GotoParser extends CommonParser {
  def goto:Parser[GOTO] = "GOTO" ~ integerNumber ^^ {case _ ~ l => GOTO(l.toInt)}
}

trait IfParser extends CommonParser with NumericExpressionParser with StatementWithoutIfParser {
  def ifS :Parser[IF] =
    "IF" ~ numericExpression ~ "THEN" ~ (ifS | statementWithoutIf) ^^ {case _ ~ c ~ _ ~ s => IF(c,s)} |
      "IF" ~ numericExpression ~ "THEN" ~ integerNumber ^^ {case _ ~ c ~ _ ~ n => IF(c,GOTO(n.toInt))} |
      "IF" ~ numericExpression ~ goto ^^ {case _ ~ c ~ s => IF(c,s)}
}

trait GosubParser extends CommonParser {
  def gosub:Parser[GOSUB] = "GOSUB" ~ integerNumber ^^ {case _ ~ l => GOSUB(l.toInt)}
}

trait ReturnParser extends CommonParser {
  def return_ :Parser[RETURN] = "RETURN" ^^ {_ => RETURN()}
}
