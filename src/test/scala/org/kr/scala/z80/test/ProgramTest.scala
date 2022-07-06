package org.kr.scala.z80.test

import org.kr.scala.z80.{Environment, Expression, Line, PRINT, Program, REM}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ProgramTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Run program line by line") {
    Scenario("Run only REM lines") {
      Given("a program consisting only REM lines")
      val program=new Program(Vector(
        new Line(5,REM(),List()),
        new Line(15,REM(),List()),
        new Line(25,REM(),List()),
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment reflects the program")
      assert(environment.getCurrentLine.contains(25))
    }
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
      assert(environment.console==List("aaaa\n","bbbb\n"))
      environment.showConsole()
    }
  }
}
