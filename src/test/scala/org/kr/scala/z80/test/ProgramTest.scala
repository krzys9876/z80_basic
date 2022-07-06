package org.kr.scala.z80.test

import org.kr.scala.z80.{Environment, Expression, Line, PRINT, Program}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ProgramTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Run program line by line") {
    Scenario("Run only print lines") {
      Given("a program consisting only print lines")
      val program=new Program(Vector(
        new Line(10,PRINT(),List(Expression("aaaa"))),
        new Line(20,PRINT(),List(Expression("bbbb")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment reflects the program")
      assert(environment.getCurrentLine.contains(20))
      println(environment.console)
      assert(environment.console==List("aaaa","bbbb"))
    }
  }
}
