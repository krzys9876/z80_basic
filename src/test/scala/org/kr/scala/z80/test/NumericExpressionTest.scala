package org.kr.scala.z80.test

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.expression.{ExprNumber, ExprOperation, ExprVariable}
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
        ExprOperation(ExprNumber(1),ExprNumber(2),"+"),
        ExprOperation(ExprNumber(8),ExprNumber(10),"-"),
        ExprOperation(ExprNumber(5),ExprNumber(8),"*"),
        ExprOperation(ExprNumber(10),ExprNumber(4),"/"),
        ExprOperation(ExprNumber(2),ExprNumber(8),"^"),
        ExprOperation(ExprNumber(3),ExprNumber(3),"="),
        ExprOperation(ExprNumber(4),ExprNumber(4),"<>"),
        ExprOperation(ExprNumber(5),ExprNumber(4),">"),
        ExprOperation(ExprNumber(8),ExprNumber(2),"<"),
        ExprOperation(ExprNumber(1),ExprNumber(-1),">="),
        ExprOperation(ExprNumber(2),ExprNumber(1),"<="),
        ExprOperation(ExprNumber(0x4AAA),ExprNumber(0x1555),"OR"),
        ExprOperation(ExprNumber(0x6CC6),ExprNumber(0x4888),"AND")
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
        ExprOperation(ExprNumber(10),ExprNumber(0),"/"),
        ExprOperation(ExprNumber(0x8000),ExprNumber(0x0001),"OR"),
        ExprOperation(ExprNumber(0x1111),ExprNumber(0x8080),"AND")
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
        ExprOperation(ExprNumber(1),ExprVariable(Variable("A")),"+"),
        ExprOperation(ExprNumber(126),ExprVariable(Variable("A")),"-"),
        ExprOperation(ExprVariable(Variable("A")),ExprNumber(2),"*"),
        ExprOperation(ExprVariable(Variable("A")),ExprNumber(2),"/"),
        ExprOperation(ExprVariable(Variable("A")),ExprNumber(2),"^"),
        ExprOperation(ExprVariable(Variable("A")),ExprVariable(Variable("A")),"="),
        ExprOperation(ExprVariable(Variable("A")),ExprVariable(Variable("A")),"<>"),
        ExprOperation(ExprVariable(Variable("A")),ExprNumber(100),">"),
        ExprOperation(ExprVariable(Variable("A")),ExprNumber(100),"<"),
        ExprOperation(ExprNumber(100),ExprVariable(Variable("A")),">="),
        ExprOperation(ExprNumber(100),ExprVariable(Variable("A")),"<="),
        ExprOperation(ExprVariable(Variable("A")),ExprNumber(0x80),"OR"),
        ExprOperation(ExprVariable(Variable("A")),ExprNumber(0x22),"AND")
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
      val e=ExprOperation(ExprNumber(1),ExprVariable(Variable("X")),"+")
      val env=Environment.empty
        .setVariable(Variable("Y"),BigDecimal(1))
      When("evaluated")
      val eVal=e.valueNum(env)
      val eErr=e.evaluate(env)
      Then("return correct numbers")
      assert(eVal.isEmpty)
      assert(eErr.isLeft)
    }
    Scenario("evaluate nested arithmetic expression (numbers only)") {
      Given("expressions representing an actual arithmetic expression: ((a+b)*(c-d))/(e^f-g)")
      val e=ExprOperation(
        ExprOperation(
          ExprOperation(
            ExprNumber(-1),
            ExprNumber(3),
            "+"),
          ExprOperation(
            ExprNumber(-5),
            ExprNumber(-8),
            "-"),
          "*"
        ),
        ExprOperation(
          ExprOperation(
            ExprNumber(2),
            ExprNumber(3),
            "^"
          ),
          ExprNumber(4),
          "-"),
        "/")
      val env=Environment.empty
      When("evaluated")
      val eVal=e.valueNum(env).get
      Then("return correct numbers")
      assert(eVal==1.5)
    }
    Scenario("evaluate nested arithmetic expression (with variables)") {
      Given("expressions representing an actual arithmetic expression: ((a+b)*(c-d))/(e^f-g)")
      val e=ExprOperation(
        ExprOperation(
          ExprOperation(
            ExprNumber(-1),
            ExprVariable(Variable("V3")),
            "+"),
          ExprOperation(
            ExprNumber(-5),
            ExprNumber(-8),
            "-"),
          "*"
        ),
        ExprOperation(
          ExprOperation(
            ExprVariable(Variable("V2")),
            ExprVariable(Variable("V3")),
            "^"
          ),
          ExprVariable(Variable("V4")),
          "-"),
        "/")
      And("variables exists in environment")
      val env=Environment.empty
        .setVariable(Variable("V2"),BigDecimal(2))
        .setVariable(Variable("V3"),BigDecimal(3))
        .setVariable(Variable("V4"),BigDecimal(4))
      When("evaluated")
      val eVal=e.valueNum(env).get
      Then("return correct numbers")
      assert(eVal==1.5)
    }
  }
}

