package org.kr.scala.z80.test

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.expression.{ExprFunction, ExprNumber, ExprOperation, ExprVariable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.expression.ExprNumber._
import org.kr.scala.z80.expression.ExprVariable._

class NumericExpressionTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("evaluate simple numeric expression") {
    Scenario("evaluate static number") {
      Given("expressions representing static numbers")
      val e = List[ExprNumber](0.0, -1, 1234.5678, 1234567890.1234567890)
      When("evaluated")
      val env = Environment.empty
      val eVals = e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals == List(0.0, -1, 1234.5678, 1234567890.1234567890))
    }
    Scenario("evaluate existing variable") {
      Given("expressions representing variables")
      val e = List[ExprVariable]("A", "ASDF", "QWERTY")
      And("variables exist in environment")
      val env = Environment.empty
        .setValue("A", BigDecimal(0.123))
        .setValue("ASDF", BigDecimal(-987654321))
        .setValue("QWERTY", BigDecimal(321.123))
      When("evaluated")
      val eVals = e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals == List(0.123, -987654321, 321.123))
    }
    Scenario("evaluate non-existing variable") {
      Given("expressions representing variables")
      val e = ExprVariable("B")
      And("variable does not exist in environment")
      val env = Environment.empty
        .setValue("A", BigDecimal(1.0))
      When("evaluated")
      val eVal = e.valueNum(env)
      Then("returns default values")
      assert(eVal.contains(0))
    }
  }
  Feature("evaluate complex expressions") {
    Scenario("evaluate binary operations (numbers only)") {
      Given("expressions representing binary operations (e.g. a+b etc.)")
      val e=List(
        ExprOperation.plus(1,2),
        ExprOperation.minus(8,10),
        ExprOperation.mul(5,8),
        ExprOperation.div(10,4),
        ExprOperation.pow(2,8),
        ExprOperation.eq(3,3),
        ExprOperation.ne(4,4),
        ExprOperation.gt(5,4),
        ExprOperation.lt(8,2),
        ExprOperation.ge(1,-1),
        ExprOperation.le(2,1),
        ExprOperation.or(0x4AAA,0x1555),
        ExprOperation.and(0x6CC6,0x4888)
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
        ExprOperation.div(10,0),
        ExprOperation.or(0x8000,0x0001),
        ExprOperation.and(0x1111,0x8080)
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
        ExprOperation.plus(1,"A"),
        ExprOperation.minus(126,"A"),
        ExprOperation.mul("A",2),
        ExprOperation.div("A",2),
        ExprOperation.pow("A",2),
        ExprOperation.eq("A","A"),
        ExprOperation.ne("A","A"),
        ExprOperation.gt("A",100),
        ExprOperation.le("A",100),
        ExprOperation.ge(100,"A"),
        ExprOperation.le(100,"A"),
        ExprOperation.or("A",0x80),
        ExprOperation.and("A",0x22)
      )
      val env=Environment.empty
        .setValue("A",BigDecimal(127))
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(128,-1,254,63.5,16129,-1.0,0.0,-1.0,0.0,0.0,-1.0,0xFF,0x22))
    }
    Scenario("evaluate binary operation for non-existing variable)") {
      Given("expression representing binary operation with a non-existing variable")
      val e=ExprOperation.plus(1,"X")
      val env=Environment.empty
        .setValue("Y",BigDecimal(1))
      When("evaluated")
      val eVal=e.valueNum(env)
      Then("non-existing variable gets default value")
      And("expression is evaluated")
      assert(eVal.contains(1.0))
    }
    Scenario("evaluate functions (numbers only)") {
      Given("expressions representing functions (e.g. SIN, COS, negation etc.)")
      val e=List(
        ExprFunction.neg(1),
        ExprFunction.sin(0),
        ExprFunction.cos(0),
        ExprFunction.abs(-1.23),
        ExprFunction.abs(2.34),
        ExprFunction.not(0x55), // bitwise not
        ExprFunction.int(1.23),
        ExprFunction.int(-1.23),
        ExprFunction.sqr(4.0)
      )
      val env=Environment.empty
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(-1.0,0.0,1.0,1.23,2.34,0xAA-256,1.0,-2.0,2.0))
    }
    Scenario("evaluate functions (variables)") {
      Given("expressions representing functions (e.g. SIN, COS, negation etc.)")
      val e=List(
        ExprFunction.neg("A"),
        ExprFunction.sin("B"),
        ExprFunction.cos("C"),
        ExprFunction.neg("D"),
        ExprFunction.abs("E"),
        ExprFunction.not("F"), // bitwise not
      )
      val env=Environment.empty
        .setValue("A",BigDecimal(-10))
        .setValue("B",BigDecimal(0))
        .setValue("C",BigDecimal(0))
        .setValue("D",BigDecimal(-3.2))
        .setValue("E",BigDecimal(4.3))
        .setValue("F",BigDecimal(0xAA))
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(10.0,0.0,1.0,3.2,4.3,0x55-256))
    }
    Scenario("evaluate incorrect functions (numbers only)") {
      Given("invalid function calls (e.g. square negative number)")
      val e=List(
        ExprFunction.not(0xEEEE),
        ExprFunction.sqr(-1.0),
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
            ExprFunction.neg("V3")),
          ExprOperation.minus(
            ExprNumber(-5),
            ExprNumber(-8))),
        ExprOperation.minus(
          ExprOperation.pow("V2",
            ExprFunction.abs("V3")),
          ExprVariable("V4")))
      And("variables exists in environment")
      val env=Environment.empty
        .setValue("V2",BigDecimal(2))
        .setValue("V3",BigDecimal(-3))
        .setValue("V4",BigDecimal(4))
      When("evaluated")
      val eVal=e.valueNum(env).get
      Then("return correct numbers")
      assert(eVal==1.5)
    }
    Scenario("evaluate relational operators") {
      Given("expressions representing relational operators (> < = <> >= <=)")
      val e=List(
        ExprOperation.eq(1,2),
        ExprOperation.ne(1,2),
        ExprOperation.gt(2,2),
        ExprOperation.lt(1,2),
        ExprOperation.ge(1,2),
        ExprOperation.le(2,2)
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
        ExprFunction.not(ExprOperation.eq(1,1)),
        ExprOperation.and(-1,-1),
        ExprOperation.or(-1,0),
        ExprOperation.or(
          ExprOperation.gt(1,2),
          ExprOperation.gt(2,1)),
        ExprOperation.and(
          ExprOperation.gt(3,1),
          ExprOperation.lt(2,1))
      )
      val env=Environment.empty
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(0,-1,-1,-1,0))
    }
  }
}