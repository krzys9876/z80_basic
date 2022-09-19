package org.kr.scala.z80.test

import org.kr.scala.z80.expression.{ExprFunction, ExprNumber, ExprOperation, ExprVariable, NumericExpression}
import org.kr.scala.z80.program.Variable
import org.kr.scala.z80.program.parser.{BaseParser, ExpressionParser}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class NumericExpressionParserTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("parse numeric expression") {
    Scenario("parse positive numbers") {
      assert(ExpressionTester("1234").contains(ExprNumber(1234.0)))
      assert(ExpressionTester("43.21").contains(ExprNumber(43.21)))
      assert(ExpressionTester("2.34E-2").contains(ExprNumber(0.0234)))
    }
    Scenario("parse negation") {
      assert(ExpressionTester("-4321").contains(ExprFunction(ExprNumber(4321.0),"-")))
      assert(ExpressionTester("-12.34").contains(ExprFunction(ExprNumber(12.34),"-")))
      assert(ExpressionTester("-5.6789E+3").contains(ExprFunction(ExprNumber(5678.9),"-")))
    }
    Scenario("do not parse text") {
      assert(ExpressionTester("1234A").isLeft)
    }
    Scenario("parse numeric variables") {
      assert(ExpressionTester("A").contains(ExprVariable(Variable("A"))))
      assert(ExpressionTester("BCD").contains(ExprVariable(Variable("BCD"))))
    }
    Scenario("do not parse text variables") {
      assert(ExpressionTester("A$").isLeft)
      assert(ExpressionTester("BCD$").isLeft)
    }
    Scenario("parse high priority functions (sin, cos, abs, negation)") {
      assert(ExpressionTester("SIN(3.14)").contains(ExprFunction(ExprNumber(3.14),"SIN")))
      assert(ExpressionTester("COS(-3.14)").contains(ExprFunction(ExprFunction(ExprNumber(3.14),"-"),"COS")))
      assert(ExpressionTester("ABS(-1.23)").contains(ExprFunction(ExprFunction(ExprNumber(1.23),"-"),"ABS")))
    }
    Scenario("parse power operator") {
      assert(ExpressionTester("1.2 ^ 3.4").contains(ExprOperation(ExprNumber(1.2),ExprNumber(3.4),"^")))
      assert(ExpressionTester("2 ^ 3 ^ 4").contains(ExprOperation(ExprOperation(ExprNumber(2),ExprNumber(3),"^"),ExprNumber(4),"^")))
      assert(ExpressionTester("3 ^ A ^ 5").contains(ExprOperation(ExprOperation(ExprNumber(3),ExprVariable(Variable("A")),"^"),ExprNumber(5),"^")))
      assert(ExpressionTester("3 ^ (A ^5)").contains(ExprOperation(ExprNumber(3),ExprOperation(ExprVariable(Variable("A")),ExprNumber(5),"^"),"^")))
    }
    Scenario("parse multiplication / division") {
      assert(ExpressionTester("1.2 * 3.4").contains(ExprOperation(ExprNumber(1.2),ExprNumber(3.4),"*")))
      assert(ExpressionTester("2 * 3 / 4").contains(ExprOperation(ExprOperation(ExprNumber(2),ExprNumber(3),"*"),ExprNumber(4),"/")))
      assert(ExpressionTester("3 / A * 5").contains(ExprOperation(ExprOperation(ExprNumber(3),ExprVariable(Variable("A")),"/"),ExprNumber(5),"*")))
      assert(ExpressionTester("3 / (A*5)").contains(ExprOperation(ExprNumber(3),ExprOperation(ExprVariable(Variable("A")),ExprNumber(5),"*"),"/")))
    }
    Scenario("parse addition / subtraction") {
      assert(ExpressionTester("1.2 - 3.4").contains(ExprOperation(ExprNumber(1.2),ExprNumber(3.4),"-")))
      assert(ExpressionTester("2 + 3 - 4").contains(ExprOperation(ExprOperation(ExprNumber(2),ExprNumber(3),"+"),ExprNumber(4),"-")))
      assert(ExpressionTester("3 - A + 5").contains(ExprOperation(ExprOperation(ExprNumber(3),ExprVariable(Variable("A")),"-"),ExprNumber(5),"+")))
      assert(ExpressionTester("3 + (A - 5)").contains(ExprOperation(ExprNumber(3),ExprOperation(ExprVariable(Variable("A")),ExprNumber(5),"-"),"+")))
    }
    Scenario("parse comparison") {
      assert(ExpressionTester("1.2=3.4").contains(ExprOperation(ExprNumber(1.2),ExprNumber(3.4),"=")))
      assert(ExpressionTester("23<>34").contains(ExprOperation(ExprNumber(23),ExprNumber(34),"<>")))
      assert(ExpressionTester("3 >= A <= 5").contains(ExprOperation(ExprOperation(ExprNumber(3),ExprVariable(Variable("A")),">="),ExprNumber(5),"<=")))
      assert(ExpressionTester("3 > (A < 5)").contains(ExprOperation(ExprNumber(3),ExprOperation(ExprVariable(Variable("A")),ExprNumber(5),"<"),">")))
    }
  }
}

case class ExpressionTester() extends BaseParser[NumericExpression] with ExpressionParser {
  def result:Parser[NumericExpression]=expr
}

object ExpressionTester {
  def apply(input:String):Either[String,NumericExpression]=ExpressionTester().process(input)
}