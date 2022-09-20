package org.kr.scala.z80.test

import org.kr.scala.z80.environment.{Dimensions, Environment, VariableCoordinates}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.program.Variable._

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
    }
  }
  Feature("handle arrays") {
    Scenario("set values of arrays for the first time") {
      Given("empty environment")
      val initEnv=Environment.empty
      When("values of 1- and 2-dimensional arrays are set")
      val coordsA11=VariableCoordinates("A1",Dimensions(List(1,2)))
      val coordsA12=VariableCoordinates("A1",Dimensions(List(2,2)))
      val coordsA21=VariableCoordinates("A2",Dimensions(List(2)))
      val coordsA22=VariableCoordinates("A2",Dimensions(List(5)))
      val afterEnv=initEnv
        .setValue(coordsA11,1)
        .setValue(coordsA12,2)
        .setValue(coordsA21,3)
        .setValue(coordsA22,4)
      Then("environment keeps these values")
      assert(afterEnv.getValue(coordsA11).contains(1))
      assert(afterEnv.getValue(coordsA12).contains(2))
      assert(afterEnv.getValue(coordsA21).contains(3))
      assert(afterEnv.getValue(coordsA22).contains(4))
    }
    Scenario("replace values of arrays") {
      Given("environment with values of 1- and 2-dimensional arrays")
      val coordsZA1=VariableCoordinates("ZA",Dimensions(List(2)))
      val coordsZA2=VariableCoordinates("ZA",Dimensions(List(3)))
      val coordsZB1=VariableCoordinates("ZB",Dimensions(List(2,4,6)))
      val coordsZB2=VariableCoordinates("ZB",Dimensions(List(3,5,7)))
      val initEnv=Environment.empty
        .setValue(coordsZA1,10)
        .setValue(coordsZA2,20)
        .setValue(coordsZB1,30)
        .setValue(coordsZB2,40)
      When("new values are set")
      val afterEnv=initEnv
        .setValue(coordsZA1,12)
        .setValue(coordsZA2,22)
        .setValue(coordsZB2,42)
      Then("environment keeps these values")
      assert(afterEnv.getValue(coordsZA1).contains(12))
      assert(afterEnv.getValue(coordsZA2).contains(22))
      assert(afterEnv.getValue(coordsZB2).contains(42))
      And("preserves values of unchanged array cells")
      assert(afterEnv.getValue(coordsZB1).contains(30))
    }
  }
}

