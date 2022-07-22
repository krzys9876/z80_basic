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
    Scenario("evaluate expression (numbers only)") {
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
  }
}

