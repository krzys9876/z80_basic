package org.kr.scala.z80.test

import org.kr.scala.z80.expression.{ExprFunction, ExprNumber, ExprOperation, ExprVariable, NumericExpression}
import org.kr.scala.z80.program.Variable
import org.kr.scala.z80.program.parser.{BaseParser, NumericExpressionParser}
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
      assert(ExpressionTester("-4321").contains(ExprFunction.neg(ExprNumber(4321.0))))
      assert(ExpressionTester("-12.34").contains(ExprFunction.neg(ExprNumber(12.34))))
      assert(ExpressionTester("-5.6789E+3").contains(ExprFunction.neg(ExprNumber(5678.9))))
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
      assert(ExpressionTester("SIN(3.14)").contains(ExprFunction.sin(ExprNumber(3.14))))
      assert(ExpressionTester("COS(-3.14)").contains(ExprFunction.cos(ExprFunction.neg(ExprNumber(3.14)))))
      assert(ExpressionTester("ABS(-1.23)").contains(ExprFunction.abs(ExprFunction.neg(ExprNumber(1.23)))))
    }
    Scenario("parse power operator") {
      assert(ExpressionTester("1.2 ^ 3.4").contains(
        ExprOperation.pow(ExprNumber(1.2),ExprNumber(3.4))))
      assert(ExpressionTester("2 ^ 3 ^ 4").contains(
        ExprOperation.pow(ExprOperation.pow(ExprNumber(2),ExprNumber(3)),ExprNumber(4))))
      assert(ExpressionTester("3 ^ A ^ 5").contains(
        ExprOperation.pow(ExprOperation.pow(ExprNumber(3),ExprVariable(Variable("A"))),ExprNumber(5))))
      assert(ExpressionTester("3 ^ (A ^5)").contains(
        ExprOperation.pow(ExprNumber(3),ExprOperation.pow(ExprVariable(Variable("A")),ExprNumber(5)))))
    }
    Scenario("parse multiplication / division") {
      assert(ExpressionTester("1.2 * 3.4").contains(
        ExprOperation.mul(ExprNumber(1.2),ExprNumber(3.4))))
      assert(ExpressionTester("2 * 3 / 4").contains(
        ExprOperation.div(ExprOperation.mul(ExprNumber(2),ExprNumber(3)),ExprNumber(4))))
      assert(ExpressionTester("3 / A * 5").contains(
        ExprOperation.mul(ExprOperation.div(ExprNumber(3),ExprVariable(Variable("A"))),ExprNumber(5))))
      assert(ExpressionTester("3 / (A*5)").contains(
        ExprOperation.div(ExprNumber(3),ExprOperation.mul(ExprVariable(Variable("A")),ExprNumber(5)))))
    }
    Scenario("parse addition / subtraction") {
      assert(ExpressionTester("1.2 - 3.4").contains(
        ExprOperation.minus(ExprNumber(1.2),ExprNumber(3.4))))
      assert(ExpressionTester("2 + 3 - 4").contains(
        ExprOperation.minus(ExprOperation.plus(ExprNumber(2),ExprNumber(3)),ExprNumber(4))))
      assert(ExpressionTester("3 - A + 5").contains(
        ExprOperation.plus(ExprOperation.minus(ExprNumber(3),ExprVariable(Variable("A"))),ExprNumber(5))))
      assert(ExpressionTester("3 + (A - 5)").contains(
        ExprOperation.plus(ExprNumber(3),ExprOperation.minus(ExprVariable(Variable("A")),ExprNumber(5)))))
    }
    Scenario("parse relational operators") {
      assert(ExpressionTester("1.2=3.4").contains(
        ExprOperation.eq(ExprNumber(1.2),ExprNumber(3.4))))
      assert(ExpressionTester("23<>34").contains(
        ExprOperation.ne(ExprNumber(23),ExprNumber(34))))
      assert(ExpressionTester("3 >= A <= 5").contains(
        ExprOperation.le(ExprOperation.ge(ExprNumber(3),ExprVariable(Variable("A"))),ExprNumber(5))))
      assert(ExpressionTester("3 > (A < 5)").contains(
        ExprOperation.gt(ExprNumber(3),ExprOperation.lt(ExprVariable(Variable("A")),ExprNumber(5)))))
    }
    Scenario("parse logical operations with precedence") {
      assert(ExpressionTester("1.2=3.4 OR 3=3").contains(
        ExprOperation.or(ExprOperation.eq(ExprNumber(1.2),ExprNumber(3.4)),ExprOperation.eq(ExprNumber(3),ExprNumber(3)))))
      assert(ExpressionTester("1.2=3.4 OR 3=3 AND 5=4").contains(
        ExprOperation.or(ExprOperation.eq(ExprNumber(1.2),ExprNumber(3.4)),
          ExprOperation.and(
            ExprOperation.eq(ExprNumber(3),ExprNumber(3)),ExprOperation.eq(ExprNumber(5),ExprNumber(4))))))
      assert(ExpressionTester("NOT 1.2=3.4 AND 3=3").contains(
        ExprOperation.and(
          ExprFunction.not(ExprOperation.eq(ExprNumber(1.2),ExprNumber(3.4))),ExprOperation.eq(ExprNumber(3),ExprNumber(3)))))
    }
  }
}

case class ExpressionTester() extends BaseParser[NumericExpression] with NumericExpressionParser {
  def result:Parser[NumericExpression]=numericExpression
}

object ExpressionTester {
  def apply(input:String):Either[String,NumericExpression]=ExpressionTester().process(input)
}