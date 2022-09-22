package org.kr.scala.z80.test

import org.kr.scala.z80.environment.{Environment, ExitCode}
import org.kr.scala.z80.expression.{ExprNumber, ExprVariable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.program.{ExprIndex, Index, Variable, VariableName, VariableStatic}

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
      val indexA11=Variable("A1",ExprIndex.static(List(1,2)))
      val indexA12=Variable("A1",ExprIndex.static(List(2,2)))
      val indexA21=Variable("A2",ExprIndex.static(List(2)))
      val indexA22=Variable("A2",ExprIndex.static(List(5)))
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
      val indexZA1=Variable("ZA",ExprIndex.static(List(2)))
      val indexZA2=Variable("ZA",ExprIndex.static(List(3)))
      val indexZB1=Variable("ZB",ExprIndex.static(List(2,4,6)))
      val indexZB2=Variable("ZB",ExprIndex.static(List(3,5,7)))
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
      val index1=Variable("XA",ExprIndex.static(List(2,1)))
      val initEnv=Environment.empty
        .setValue(index1,100)
      When("new values are set with invalid dimension (outside of range)")
      val index2=Variable("XA",ExprIndex.static(List(2,11)))
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
      val index1=Variable("AR",ExprIndex(List(ExprVariable("I"))))
      val afterEnv=initEnv
        .setValue(index1,100)
      Then("the value is stored to environment")
      val indexStatic1=Variable("AR",ExprIndex.static(List(2)))
      assert(afterEnv.getValue(indexStatic1).contains(100))
    }
    Scenario("read a value from array with variable as indexes") {
      Given("environment with an array variable")
      val indexStatic1=Variable("ARR",ExprIndex.static(List(3)))
      val env=Environment.empty
        .setValue("J",BigDecimal.valueOf(3))
        .setValue(indexStatic1,BigDecimal.valueOf(200))
      When("a value is read from an array using the variable as index")
      val index2=Variable("ARR",ExprIndex(List(ExprVariable("J"))))
      Then("the value is stored to environment")
      assert(env.getValue(index2).contains(200))
    }
    Scenario("set array dimensions (DIM)") {
      Given("environment with declared variable dimension greater than default (10)")
      val indexDim1=Variable(VariableName("ARR"),ExprIndex.static(List(20,30)))
      val index1=Variable(VariableName("ARR"),ExprIndex.static(List(18,30)))
      val env=Environment.empty
        .setArrayDim(indexDim1)
        .setValue(index1,1.0)
      When("a value is read from an array using index greater than default (10)")
      Then("the value is stored to environment")
      assert(env.getValue(index1).contains(1.0))
    }
    Scenario("read valid and invalid array index") {
      Given("environment with declared variable dimension")
      val indexDim1=Variable(VariableName("ARR"),ExprIndex.static(List(15,7)))
      val indexValid1=Variable(VariableName("ARR"),ExprIndex.static(List(15,0)))
      val indexValid2=Variable(VariableName("ARR"),ExprIndex.static(List(0,7)))
      val indexInvalid1=Variable(VariableName("ARR"),ExprIndex.static(List(16,3)))
      val indexInvalid2=Variable(VariableName("ARR"),ExprIndex.static(List(9,8)))
      val env=Environment.empty
        .setArrayDim(indexDim1)
      When("a value is read from an array using index within declared range")
      Then("default value is returned")
      assert(env.getValue(indexValid1).contains(0))
      assert(env.getValue(indexValid2).contains(0))
      When("a value is read from an array using greater index than declared")
      Then("error is returned - invalid index")
      assert(env.getValue(indexInvalid1).swap.contains(ExitCode.INVALID_ARRAY_INDEX))
      assert(env.getValue(indexInvalid2).swap.contains(ExitCode.INVALID_ARRAY_INDEX))
    }
    Scenario("store valid and invalid array index") {
      Given("environment with declared variable dimension")
      val index1=Variable(VariableName("ARR"),ExprIndex.static(List(15,7)))
      val indexValid1=Variable(VariableName("ARR"),ExprIndex.static(List(15,0)))
      val indexValid2=Variable(VariableName("ARR"),ExprIndex.static(List(0,7)))
      val indexInvalid1=Variable(VariableName("ARR"),ExprIndex.static(List(16,3)))
      val indexInvalid2=Variable(VariableName("ARR"),ExprIndex.static(List(9,8)))
      val env=Environment.empty
        .setArrayDim(index1)
      When("a value is stored to an array using index within declared range")
      Then("normal exit code is returned")
      assert(env.setValue(indexValid1,1.0).exitCode==ExitCode.NORMAL)
      assert(env.setValue(indexValid2,1.0).exitCode==ExitCode.NORMAL)
      When("a value is stored to an array using greater index than declared")
      Then("error is returned - invalid index")
      assert(env.setValue(indexInvalid1,1.0).exitCode==ExitCode.INVALID_ARRAY_INDEX)
      assert(env.setValue(indexInvalid2,1.0).exitCode==ExitCode.INVALID_ARRAY_INDEX)
    }
  }
  Feature("default variable values") {
    Scenario("read undeclared variables of different types") {
      Given("empty environment")
      val env=Environment.empty
        //.setValue("A",0)
      When("a numeric variable is read")
      Then("its value is 0")
      assert(env.getValue("A").contains(0))
    }
  }
}
