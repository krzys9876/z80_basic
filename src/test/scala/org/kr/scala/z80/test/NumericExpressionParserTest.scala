package org.kr.scala.z80.test

import org.kr.scala.z80.expression.{ExprFunction, ExprNumber, ExprOperation, ExprVariable, NumericExpression}
import org.kr.scala.z80.parser.{BaseParser, NumericExpressionParser}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.expression.ExprVariable._
import org.kr.scala.z80.expression.ExprNumber._

class NumericExpressionParserTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("parse numeric expression") {
    Scenario("parse positive numbers") {
      assert(ExpressionTester("1234").contains(ExprNumber(1234.0)))
      assert(ExpressionTester("43.21").contains(ExprNumber(43.21)))
      assert(ExpressionTester("2.34E-2").contains(ExprNumber(0.0234)))
    }
    Scenario("parse negation") {
      assert(ExpressionTester("-4321").contains(ExprFunction.neg(4321.0)))
      assert(ExpressionTester("-12.34").contains(ExprFunction.neg(12.34)))
      assert(ExpressionTester("-5.6789E+3").contains(ExprFunction.neg(5678.9)))
    }
    Scenario("do not parse text") {
      assert(ExpressionTester("1234A").isLeft)
    }
    Scenario("parse numeric variables") {
      assert(ExpressionTester("A").contains(ExprVariable("A")))
      assert(ExpressionTester("BCD").contains(ExprVariable("BCD")))
    }
    Scenario("do not parse text variables") {
      assert(ExpressionTester("A$").isLeft)
      assert(ExpressionTester("BCD$").isLeft)
    }
    Scenario("parse functions (sin, cos, abs, negation etc.)") {
      assert(ExpressionTester("SIN(3.14)").contains(ExprFunction.sin(3.14)))
      assert(ExpressionTester("COS(-3.14)").contains(ExprFunction.cos(ExprFunction.neg(3.14))))
      assert(ExpressionTester("ABS(-1.23)").contains(ExprFunction.abs(ExprFunction.neg(1.23))))
      assert(ExpressionTester("SQR(4)").contains(ExprFunction.sqr(4)))
      assert(ExpressionTester("INT(3.4)").contains(ExprFunction.int(3.4)))
    }
    Scenario("parse power operator") {
      assert(ExpressionTester("1.2 ^ 3.4").contains(
        ExprOperation.pow(1.2,3.4)))
      assert(ExpressionTester("2 ^ 3 ^ 4").contains(
        ExprOperation.pow(ExprOperation.pow(2,3),4)))
      assert(ExpressionTester("3 ^ A ^ 5").contains(
        ExprOperation.pow(ExprOperation.pow(3,"A"),5)))
      assert(ExpressionTester("3 ^ (A ^5)").contains(
        ExprOperation.pow(3,ExprOperation.pow("A",5))))
    }
    Scenario("parse multiplication / division") {
      assert(ExpressionTester("1.2 * 3.4").contains(
        ExprOperation.mul(1.2,3.4)))
      assert(ExpressionTester("2 * 3 / 4").contains(
        ExprOperation.div(ExprOperation.mul(2,3),4)))
      assert(ExpressionTester("3 / A * 5").contains(
        ExprOperation.mul(ExprOperation.div(3,"A"),5)))
      assert(ExpressionTester("3 / (A*5)").contains(
        ExprOperation.div(3,ExprOperation.mul("A",5))))
    }
    Scenario("parse addition / subtraction") {
      assert(ExpressionTester("1.2 - 3.4").contains(
        ExprOperation.minus(1.2,3.4)))
      assert(ExpressionTester("2 + 3 - 4").contains(
        ExprOperation.minus(ExprOperation.plus(2,3),4)))
      assert(ExpressionTester("3 - A + 5").contains(
        ExprOperation.plus(ExprOperation.minus(3,"A"),5)))
      assert(ExpressionTester("3 + (A - 5)").contains(
        ExprOperation.plus(3,ExprOperation.minus("A",5))))
    }
    Scenario("parse relational operators") {
      assert(ExpressionTester("1.2=3.4").contains(
        ExprOperation.eq(1.2,3.4)))
      assert(ExpressionTester("23<>34").contains(
        ExprOperation.ne(23,34)))
      assert(ExpressionTester("3 >= A <= 5").contains(
        ExprOperation.le(ExprOperation.ge(3,"A"),5)))
      assert(ExpressionTester("3 > (A < 5)").contains(
        ExprOperation.gt(3,ExprOperation.lt("A",5))))
    }
    Scenario("parse logical operations with precedence") {
      assert(ExpressionTester("1.2=3.4 OR 3=3").contains(
        ExprOperation.or(ExprOperation.eq(1.2,3.4),ExprOperation.eq(3,3))))
      assert(ExpressionTester("1.2=3.4 OR 3=3 AND 5=4").contains(
        ExprOperation.or(ExprOperation.eq(1.2,3.4),
          ExprOperation.and(
            ExprOperation.eq(3,3),ExprOperation.eq(5,4)))))
      assert(ExpressionTester("NOT 1.2=3.4 AND 3=3").contains(
        ExprOperation.and(
          ExprFunction.not(ExprOperation.eq(1.2,3.4)),ExprOperation.eq(3,3))))
    }
  }
}

case class ExpressionTester() extends BaseParser[NumericExpression] with NumericExpressionParser {
  def result:Parser[NumericExpression]=numericExpression
}

object ExpressionTester {
  def apply(input:String):Either[String,NumericExpression]=ExpressionTester().process(input)
}