package org.kr.scala.z80.test

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.expression.{ExprFunction, ExprNumber, ExprOperation, ExprVariable}
import org.kr.scala.z80.program.Variable
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class NumericExpressionTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("evaluate simple numeric expression") {
    Scenario("evaluate static number") {
      Given("expressions representing static numbers")
      val e = List(ExprNumber(0.0), ExprNumber(-1), ExprNumber(1234.5678), ExprNumber(1234567890.1234567890))
      When("evaluated")
      val env = Environment.empty
      val eVals = e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals == List(0.0, -1, 1234.5678, 1234567890.1234567890))
    }
    Scenario("evaluate existing variable") {
      Given("expressions representing variables")
      val e = List(ExprVariable(Variable("A")), ExprVariable(Variable("ASDF")), ExprVariable(Variable("QWERTY")))
      And("variables exist in environment")
      val env = Environment.empty
        .setVariable(Variable("A"), BigDecimal(0.123))
        .setVariable(Variable("ASDF"), BigDecimal(-987654321))
        .setVariable(Variable("QWERTY"), BigDecimal(321.123))
      When("evaluated")
      val eVals = e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals == List(0.123, -987654321, 321.123))
    }
    Scenario("evaluate non-existing variable") {
      Given("expressions representing variables")
      val e = ExprVariable(Variable("B"))
      And("variable does not exist in environment")
      val env = Environment.empty
        .setVariable(Variable("A"), BigDecimal(1.0))
      When("evaluated")
      val eVal = e.valueNum(env)
      val eErr = e.evaluate(env)
      Then("returns error message and empty value")
      assert(eVal.isEmpty)
      assert(eErr.isLeft)
    }
  }
  Feature("evaluate complex expressions") {
    Scenario("evaluate binary operations (numbers only)") {
      Given("expressions representing binary operations (e.g. a+b etc.)")
      val e=List(
        ExprOperation.plus(ExprNumber(1),ExprNumber(2)),
        ExprOperation.minus(ExprNumber(8),ExprNumber(10)),
        ExprOperation.mul(ExprNumber(5),ExprNumber(8)),
        ExprOperation.div(ExprNumber(10),ExprNumber(4)),
        ExprOperation.pow(ExprNumber(2),ExprNumber(8)),
        ExprOperation.eq(ExprNumber(3),ExprNumber(3)),
        ExprOperation.ne(ExprNumber(4),ExprNumber(4)),
        ExprOperation.gt(ExprNumber(5),ExprNumber(4)),
        ExprOperation.lt(ExprNumber(8),ExprNumber(2)),
        ExprOperation.ge(ExprNumber(1),ExprNumber(-1)),
        ExprOperation.le(ExprNumber(2),ExprNumber(1)),
        ExprOperation.or(ExprNumber(0x4AAA),ExprNumber(0x1555)),
        ExprOperation.and(ExprNumber(0x6CC6),ExprNumber(0x4888))
      )
      val env=Environment.empty
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(3.0,-2.0,40.0,2.5,256.0,-1.0,0.0,-1.0,0.0,-1.0,0.0,0x5FFF,0x4880))
    }
    Scenario("evaluate incorrect binary operations (numbers only)") {
      Given("expressions representing binary operations (e.g. a+b etc.)")
      val e=List(
        ExprOperation.div(ExprNumber(10),ExprNumber(0)),
        ExprOperation.or(ExprNumber(0x8000),ExprNumber(0x0001)),
        ExprOperation.and(ExprNumber(0x1111),ExprNumber(0x8080))
      )
      val env=Environment.empty
      When("evaluated")
      val eVals=e.map(_.valueNum(env))
      val eErrs=e.map(_.evaluate(env))
      Then("return correct numbers")
      assert(eVals.forall(_.isEmpty))
      assert(eErrs.forall(_.isLeft))
    }
    Scenario("evaluate binary operations (numbers and variables)") {
      Given("expressions representing binary operations (e.g. a+b etc.)")
      val e=List(
        ExprOperation.plus(ExprNumber(1),ExprVariable(Variable("A"))),
        ExprOperation.minus(ExprNumber(126),ExprVariable(Variable("A"))),
        ExprOperation.mul(ExprVariable(Variable("A")),ExprNumber(2)),
        ExprOperation.div(ExprVariable(Variable("A")),ExprNumber(2)),
        ExprOperation.pow(ExprVariable(Variable("A")),ExprNumber(2)),
        ExprOperation.eq(ExprVariable(Variable("A")),ExprVariable(Variable("A"))),
        ExprOperation.ne(ExprVariable(Variable("A")),ExprVariable(Variable("A"))),
        ExprOperation.gt(ExprVariable(Variable("A")),ExprNumber(100)),
        ExprOperation.le(ExprVariable(Variable("A")),ExprNumber(100)),
        ExprOperation.ge(ExprNumber(100),ExprVariable(Variable("A"))),
        ExprOperation.le(ExprNumber(100),ExprVariable(Variable("A"))),
        ExprOperation.or(ExprVariable(Variable("A")),ExprNumber(0x80)),
        ExprOperation.and(ExprVariable(Variable("A")),ExprNumber(0x22))
      )
      val env=Environment.empty
        .setVariable(Variable("A"),BigDecimal(127))
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(128,-1,254,63.5,16129,-1.0,0.0,-1.0,0.0,0.0,-1.0,0xFF,0x22))
    }
    Scenario("evaluate binary operation for non-existing variable)") {
      Given("expression representing binary operation with a non-existing variable")
      val e=ExprOperation.plus(ExprNumber(1),ExprVariable(Variable("X")))
      val env=Environment.empty
        .setVariable(Variable("Y"),BigDecimal(1))
      When("evaluated")
      val eVal=e.valueNum(env)
      val eErr=e.evaluate(env)
      Then("return error")
      assert(eVal.isEmpty)
      assert(eErr.isLeft)
    }
    Scenario("evaluate functions (numbers only)") {
      Given("expressions representing functions (e.g. SIN, COS, negation etc.)")
      val e=List(
        ExprFunction.neg(ExprNumber(1)),
        ExprFunction.sin(ExprNumber(0)),
        ExprFunction.cos(ExprNumber(0)),
        ExprFunction.abs(ExprNumber(-1.23)),
        ExprFunction.abs(ExprNumber(2.34)),
        ExprFunction.not(ExprNumber(0x55)), // bitwise not
      )
      val env=Environment.empty
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(-1.0,0.0,1.0,1.23,2.34,0xAA-256))
    }
    Scenario("evaluate functions (variables)") {
      Given("expressions representing functions (e.g. SIN, COS, negation etc.)")
      val e=List(
        ExprFunction.neg(ExprVariable(Variable("A"))),
        ExprFunction.sin(ExprVariable(Variable("B"))),
        ExprFunction.cos(ExprVariable(Variable("C"))),
        ExprFunction.neg(ExprVariable(Variable("D"))),
        ExprFunction.abs(ExprVariable(Variable("E"))),
        ExprFunction.not(ExprVariable(Variable("F"))), // bitwise not
      )
      val env=Environment.empty
        .setVariable(Variable("A"),BigDecimal(-10))
        .setVariable(Variable("B"),BigDecimal(0))
        .setVariable(Variable("C"),BigDecimal(0))
        .setVariable(Variable("D"),BigDecimal(-3.2))
        .setVariable(Variable("E"),BigDecimal(4.3))
        .setVariable(Variable("F"),BigDecimal(0xAA))
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(10.0,0.0,1.0,3.2,4.3,0x55-256))
    }
    Scenario("evaluate incorrect functions (numbers only)") {
      Given("expressions representing functions (e.g. SIN, COS, negation etc.)")
      val e=List(
        ExprFunction.not(ExprNumber(0xEEEE)),
      )
      val env=Environment.empty
      When("evaluated")
      val eVals=e.map(_.valueNum(env))
      val eErrs=e.map(_.evaluate(env))
      Then("return error")
      assert(eVals.forall(_.isEmpty))
      assert(eErrs.forall(_.isLeft))
    }
    Scenario("evaluate nested arithmetic expression (numbers only)") {
      Given("expressions representing an actual arithmetic expression: ((a+(-b))*(c-d))/(e^ABS(f)-g)")
      val e=ExprOperation.div(
        ExprOperation.mul(
          ExprOperation.plus(
            ExprNumber(-1),
            ExprFunction.neg(ExprNumber(-3))),
          ExprOperation.minus(
            ExprNumber(-5),
            ExprNumber(-8))),
        ExprOperation.minus(
          ExprOperation.pow(
            ExprNumber(2),
            ExprFunction.abs(ExprNumber(-3))),
          ExprNumber(4)))
      val env=Environment.empty
      When("evaluated")
      val eVal=e.valueNum(env).get
      Then("return correct numbers")
      assert(eVal==1.5)
    }
    Scenario("evaluate nested arithmetic expression (with variables)") {
      Given("expressions representing an actual arithmetic expression: ((a+(-b))*(c-d))/(e^ABS(f)-g)")
      val e=ExprOperation.div(
        ExprOperation.mul(
          ExprOperation.plus(
            ExprNumber(-1),
            ExprFunction.neg(
              ExprVariable(Variable("V3")))),
          ExprOperation.minus(
            ExprNumber(-5),
            ExprNumber(-8))),
        ExprOperation.minus(
          ExprOperation.pow(
            ExprVariable(Variable("V2")),
            ExprFunction.abs(
              ExprVariable(Variable("V3")))),
          ExprVariable(Variable("V4"))))
      And("variables exists in environment")
      val env=Environment.empty
        .setVariable(Variable("V2"),BigDecimal(2))
        .setVariable(Variable("V3"),BigDecimal(-3))
        .setVariable(Variable("V4"),BigDecimal(4))
      When("evaluated")
      val eVal=e.valueNum(env).get
      Then("return correct numbers")
      assert(eVal==1.5)
    }
    Scenario("evaluate relational operators") {
      Given("expressions representing relational operators (> < = <> >= <=)")
      val e=List(
        ExprOperation.eq(ExprNumber(1),ExprNumber(2)),
        ExprOperation.ne(ExprNumber(1),ExprNumber(2)),
        ExprOperation.gt(ExprNumber(2),ExprNumber(2)),
        ExprOperation.lt(ExprNumber(1),ExprNumber(2)),
        ExprOperation.ge(ExprNumber(1),ExprNumber(2)),
        ExprOperation.le(ExprNumber(2),ExprNumber(2))
      )
      val env=Environment.empty
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(0,-1,0,-1,0,-1))
    }
    Scenario("evaluate logical functions") {
      Given("expressions representing logical functions (not, and, or)")
      val e=List(
        ExprFunction.not(ExprOperation.eq(ExprNumber(1),ExprNumber(1))),
        ExprOperation.and(ExprNumber(-1),ExprNumber(-1)),
        ExprOperation.or(ExprNumber(-1),ExprNumber(0)),
        ExprOperation.or(
          ExprOperation.gt(ExprNumber(1),ExprNumber(2)),
          ExprOperation.gt(ExprNumber(2),ExprNumber(1))),
        ExprOperation.and(
          ExprOperation.gt(ExprNumber(3),ExprNumber(1)),
          ExprOperation.lt(ExprNumber(2),ExprNumber(1)))
      )
      val env=Environment.empty
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(0,-1,-1,-1,0))
    }
  }
}