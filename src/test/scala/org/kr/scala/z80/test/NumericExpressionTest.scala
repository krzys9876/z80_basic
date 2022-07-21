package org.kr.scala.z80.test

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.expression.{ExprNumber, ExprVariable}
import org.kr.scala.z80.program.Variable
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class NumericExpressionTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("evaluate simple numeric expression") {
    Scenario("evaluate static number") {
      Given("expressions representing static numbers")
      val e=List(ExprNumber(0.0),ExprNumber(-1),ExprNumber(1234.5678),ExprNumber(1234567890.1234567890))
      When("evaluated")
      val env=Environment.empty
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(0.0,-1,1234.5678,1234567890.1234567890))
    }
    Scenario("evaluate variable") {
      Given("expressions representing variables")
      val e=List(ExprVariable(Variable("A")),ExprVariable(Variable("ASDF")),ExprVariable(Variable("QWERTY")))
      And("variables exist in environment")
      val env=Environment.empty
        .setVariable(Variable("A"),BigDecimal(0.123))
        .setVariable(Variable("ASDF"),BigDecimal(-987654321))
        .setVariable(Variable("QWERTY"),BigDecimal(321.123))
      When("evaluated")
      val eVals=e.map(_.valueNum(env).get)
      Then("return correct numbers")
      assert(eVals==List(0.123,-987654321,321.123))
    }
  }
}

