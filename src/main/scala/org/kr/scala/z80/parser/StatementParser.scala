package org.kr.scala.z80.parser

import org.kr.scala.z80.expression.{BlankTextExpr, ExprVariable, Expression, StaticTextExpr}
import org.kr.scala.z80.program.{Assignment, DATA, DIM, FOR, GOSUB, GOTO, IF, Index, LET, NEXT, NumericAssignment, PRINT, PrintableToken, REM, RETURN, Statement, Variable}

import scala.util.parsing.combinator.JavaTokenParsers

trait StatementParser extends CommonParser with StatementWithoutIfParser with IfParser {
  def statement: Parser[Statement] = statementWithoutIf | if_
}

trait CommonParser extends JavaTokenParsers {
  def integerNumber: Parser[String] = """(\d+)""".r
  def anyText: Parser[String] = """(.*)""".r
  def anyTextWoComma: Parser[String] = """([^, ]*)""".r
  def anyTextQuoted:Parser[String] = stringLiteral ^^ stripQuotes
  def emptyString:Parser[String] = """(^$)""".r

  private def stripFirstAndLastChar(t:String):String = t.substring(1,t.length-1)
  private def stripQuotes(t:String):String =
    if(t.startsWith("\"") && t.endsWith("\"")) stripFirstAndLastChar(t) else t
}

// avoid circular inheritance (IfParser cannot inherit from StatementParser, which must inherit from IfParser)
trait StatementWithoutIfParser extends CommonParser with RemParser with PrintParser with ForParser with NextParser
  with LetParser with GotoParser with GosubParser with ReturnParser with DimParser with DataParser {
  def statementWithoutIf: Parser[Statement] = rem | print | for_ | next | let | goto | gosub | return_ | dim |
    data
}

trait RemParser extends CommonParser {
  def rem:Parser[REM] = "REM" ~> (anyText | emptyString) ^^ {t => REM(t)}
}

trait PrintParser extends CommonParser with NumericExpressionParser {
  def print:Parser[PRINT] = "PRINT" ~> opt(tokens) ^^ {t => PRINT(t.getOrElse(List()).toVector)}

  private def staticTextExpr:Parser[StaticTextExpr] = anyTextQuoted ^^ StaticTextExpr
  private def token:Parser[Expression]=numericExpression | staticTextExpr
  private def tokenSeparator:Parser[String]=";" | ","
  private def tokenSep:Parser[PrintableToken]=tokenSeparator ~ token ^^ {case s ~ t => PrintableToken(Some(s),t)}
  private def tokens:Parser[List[PrintableToken]]=
    token ~ opt(rep(tokenSep)) ~ opt(rep(tokenSeparator)) ^^ {case t ~ l ~ s =>
      List(PrintableToken(None,t)) ++
        l.getOrElse(List()) ++
        s.getOrElse(List()).map(sep=>PrintableToken(Some(sep),BlankTextExpr))}
}

trait VariableParser extends CommonParser {
  def numVariable:Parser[Variable]=numVariableName ^^ {Variable.fromString}
  def textVariable:Parser[Variable]=textVariableName ^^ {Variable.fromString}
  def numVariableName:Parser[String]="""([A-Z]+)""".r
  def textVariableName:Parser[String]="""([A-Z]+\$)""".r
}

trait NextParser extends CommonParser with VariableParser {
  def next:Parser[NEXT] =
    "NEXT" ~> numVariable ^^ {v => NEXT(Some(v))} |
      "NEXT" ~> emptyString ^^ {_ => NEXT()}
}

trait ForParser extends CommonParser with VariableParser with AssignmentParser {
  def for_ :Parser[FOR] =
    ("FOR" ~> numericAssignment) ~ ("TO" ~> numericExpression) ~ ("STEP" ~> numericExpression)  ^^ {
      case from ~ to ~ step => FOR(from,to,Some(step))} |
      ("FOR" ~> numericAssignment) ~ ("TO" ~> numericExpression) ^^ { case from ~ to => FOR(from,to)}
}

trait AssignmentParser extends VariableParser with NumericExpressionParser {
  def numericAssignment:Parser[NumericAssignment] = numVariable ~ ("=" ~> numericExpression) ^^ {
    case v ~ e => NumericAssignment(v,e)
  }
  def numericArrayAssignment:Parser[NumericAssignment] = numArray ~ ("=" ~> numericExpression) ^^ {
    case v ~ e => NumericAssignment(v,e)
  }
  //TODO: extend text assignment with text expressions (after it is implemented)
  def textAssignment:Parser[Assignment] =
    (textArray | textVariable) ~ ("=" ~> anyTextQuoted) ^^ {case v ~ e => Assignment(v,StaticTextExpr(e))} |
      (textArray | textVariable) ~ ("=" ~> (textArray | textVariable)) ^^ {case v ~ e => Assignment(v,ExprVariable(e))}
}

trait LetParser extends AssignmentParser {
  def let:Parser[LET] = (textAssignment | numericArrayAssignment | numericAssignment) ^^ {LET(_)} |
    "LET" ~> (textAssignment | numericArrayAssignment | numericAssignment) ^^ {LET(_)}
}

trait GotoParser extends CommonParser {
  def goto:Parser[GOTO] = "GOTO" ~> integerNumber ^^ {l => GOTO(l.toInt)}
}

trait IfParser extends CommonParser with NumericExpressionParser with StatementWithoutIfParser {
  def if_ :Parser[IF] =
    ("IF" ~> numericExpression) ~ ("THEN" ~> (if_ | statementWithoutIf)) ^^ {case c ~ s => IF(c,s)} |
      ("IF" ~> numericExpression) ~ ("THEN" ~> integerNumber) ^^ {case c ~ n => IF(c,GOTO(n.toInt))} |
      ("IF" ~> numericExpression) ~ goto ^^ {case c ~ s => IF(c,s)}
}

trait GosubParser extends CommonParser {
  def gosub:Parser[GOSUB] = "GOSUB" ~> integerNumber ^^ {l => GOSUB(l.toInt)}
}

trait ReturnParser extends CommonParser {
  def return_ :Parser[RETURN] = "RETURN" ^^ {_ => RETURN()}
}

trait DimParser extends CommonParser with VariableParser {
  def dim :Parser[DIM] = "DIM" ~> numVariable ~ ("(" ~> staticIndex <~ ")") ^^ {case v ~ i  => DIM(v.asStatic(Index(i)))}
  private def staticIndexSingle:Parser[Int]=integerNumber ^^ {_.toInt}
  private def staticIndex:Parser[List[Int]]=rep1sep(staticIndexSingle,",")
}

trait DataParser extends CommonParser {
  def data :Parser[DATA] = "DATA" ~> listOfValues ^^ DATA
  private def number[BigDecimal]=floatingPointNumber ^^ {n=>BigDecimal(n.toDouble)}
  private def dataValue:Parser[Any]=number | anyTextQuoted | anyTextWoComma
  private def listOfValues:Parser[List[Any]]=rep1sep(dataValue,",")
}
