package org.kr.scala.z80.test

import org.kr.scala.z80.environment.{Environment, ExitCode}
import org.kr.scala.z80.expression.ExprVariable
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.program.{ExprIndex, Index, VariableIndex}

class VariableTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("simple variables") {
    Scenario("set variable value for the first time") {
      Given("empty environment")
      val initEnv=Environment.empty
      When("values are set for numeric and text variables")
      val afterEnv=initEnv
        .setValue("X",123.456)
        .setValue("Y$", "xyz!")
      Then("environment keeps these values")
      assert(afterEnv.getValue("X").contains(123.456))
      assert(afterEnv.getValue("Y$").contains("xyz!"))
      assert(afterEnv.exitCode==ExitCode.NORMAL)
    }
    Scenario("replace value of a variable") {
      Given("environment with numeric and text variables")
      val initEnv=Environment.empty
        .setValue("A",-200)
        .setValue("B$", "TEXT@")
        .setValue("ZZ", 35.1)
        .setValue("ZA$", "Pi")
      When("new values are set")
      val afterEnv=initEnv
        .setValue("A",-300)
        .setValue("B$", "other#")
      Then("environment keeps these values")
      assert(afterEnv.getValue("A").contains(-300))
      assert(afterEnv.getValue("B$").contains("other#"))
      And("preserves values of unchanged variables")
      assert(afterEnv.getValue("ZZ").contains(35.1))
      assert(afterEnv.getValue("ZA$").contains("Pi"))
      assert(afterEnv.exitCode==ExitCode.NORMAL)
    }
  }
  Feature("handle arrays") {
    Scenario("set values of arrays for the first time (default size)") {
      Given("empty environment")
      val initEnv=Environment.empty
      When("values of 1- and 2-dimensional arrays are set")
      val indexA11=VariableIndex("A1",ExprIndex.static(List(1,2)))
      val indexA12=VariableIndex("A1",ExprIndex.static(List(2,2)))
      val indexA21=VariableIndex("A2",ExprIndex.static(List(2)))
      val indexA22=VariableIndex("A2",ExprIndex.static(List(5)))
      val afterEnv=initEnv
        .setValue(indexA11,1)
        .setValue(indexA12,2)
        .setValue(indexA21,3)
        .setValue(indexA22,4)
      Then("environment keeps these values")
      assert(afterEnv.getValue(indexA11).contains(1))
      assert(afterEnv.getValue(indexA12).contains(2))
      assert(afterEnv.getValue(indexA21).contains(3))
      assert(afterEnv.getValue(indexA22).contains(4))
      assert(afterEnv.exitCode==ExitCode.NORMAL)
    }
    Scenario("replace values of arrays (default size)") {
      Given("environment with values of 1- and 2-dimensional arrays")
      val indexZA1=VariableIndex("ZA",ExprIndex.static(List(2)))
      val indexZA2=VariableIndex("ZA",ExprIndex.static(List(3)))
      val indexZB1=VariableIndex("ZB",ExprIndex.static(List(2,4,6)))
      val indexZB2=VariableIndex("ZB",ExprIndex.static(List(3,5,7)))
      val initEnv=Environment.empty
        .setValue(indexZA1,10)
        .setValue(indexZA2,20)
        .setValue(indexZB1,30)
        .setValue(indexZB2,40)
      When("new values are set")
      val afterEnv=initEnv
        .setValue(indexZA1,12)
        .setValue(indexZA2,22)
        .setValue(indexZB2,42)
      Then("environment keeps these values")
      assert(afterEnv.getValue(indexZA1).contains(12))
      assert(afterEnv.getValue(indexZA2).contains(22))
      assert(afterEnv.getValue(indexZB2).contains(42))
      And("preserves values of unchanged array cells")
      assert(afterEnv.getValue(indexZB1).contains(30))
      assert(afterEnv.exitCode==ExitCode.NORMAL)
    }
    Scenario("replace values of arrays with dimensions out of range (default size)") {
      Given("environment with values of 1- and 2-dimensional arrays")
      val index1=VariableIndex("XA",ExprIndex.static(List(2,1)))
      val initEnv=Environment.empty
        .setValue(index1,100)
      When("new values are set with invalid dimension (outside of range)")
      val index2=VariableIndex("XA",ExprIndex.static(List(2,11)))
      val afterEnv=initEnv
        .setValue(index2,110)
      Then("environment ends with exit code - invalid array index")
      assert(afterEnv.exitCode==ExitCode.INVALID_ARRAY_INDEX)
    }
    Scenario("assign a value to array with variable as indexes") {
      Given("environment with a variable")
      val initEnv=Environment.empty
        .setValue("I",BigDecimal.valueOf(2))
      When("a value in array is set using the variable as index")
      val index1=VariableIndex("AR",ExprIndex(List(ExprVariable("I"))))
      val afterEnv=initEnv
        .setValue(index1,100)
      Then("the value is stored to environment")
      val indexStatic1=VariableIndex("AR",ExprIndex.static(List(2)))
      assert(afterEnv.getValue(indexStatic1).contains(100))
    }
    Scenario("read a value from array with variable as indexes") {
      Given("environment with an array variable")
      val indexStatic1=VariableIndex("ARR",ExprIndex.static(List(3)))
      val env=Environment.empty
        .setValue("J",BigDecimal.valueOf(3))
        .setValue(indexStatic1,BigDecimal.valueOf(200))
      When("a value is read from an array using the variable as index")
      val index2=VariableIndex("ARR",ExprIndex(List(ExprVariable("J"))))
      Then("the value is stored to environment")
      assert(env.getValue(index2).contains(200))
    }
  }
}

