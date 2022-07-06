package org.kr.scala.z80.test

import org.kr.scala.z80.{Assignment, Environment, Expression, LET, Line, PRINT, Program, REM, Variable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ProgramTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Run program line by line") {
    Scenario("Run only REM lines") {
      Given("a program consisting of REM lines")
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
    Scenario("Run assignment to a variable") {
      Given("a program consisting of assignment lines")
      val program=new Program(Vector(
        new Line(30,LET(),List(Assignment(Variable("A"),Expression(123L)))),
        new Line(35,LET(),List(Assignment(Variable("B"),Expression(234.123)))),
        new Line(40,LET(),List(Assignment(Variable("C"),Expression("qwerty")))),
        new Line(45,LET(),List(Assignment(Variable("D"),Expression(false))))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment reflects the program")
      assert(environment.getCurrentLine.contains(45))
      assert(environment.getValue(Variable("A")).contains(Expression(123L)))
      assert(environment.getValue(Variable("B")).contains(Expression(234.123)))
      assert(environment.getValue(Variable("C")).contains(Expression("qwerty")))
      assert(environment.getValue(Variable("D")).contains(Expression(false)))
    }
  }
}
