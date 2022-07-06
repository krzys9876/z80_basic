package org.kr.scala.z80.test

import org.kr.scala.z80.{Assignment, Environment, FOR, LET, Line, NEXT, PRINT, Program, REM, Result, TO, Variable}
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
        new Line(10,PRINT(),List(Result("aaaa"))),
        new Line(20,PRINT(),List(Result("bbbb")))
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
        new Line(30,LET(),List(Assignment(Variable("A"),Result(123L)))),
        new Line(35,LET(),List(Assignment(Variable("B"),Result(234.123)))),
        new Line(40,LET(),List(Assignment(Variable("C"),Result("qwerty")))),
        new Line(45,LET(),List(Assignment(Variable("D"),Result(false))))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment reflects the program")
      assert(environment.getCurrentLine.contains(45))
      assert(environment.getValue(Variable("A")).contains(Result(123L)))
      assert(environment.getValue(Variable("B")).contains(Result(234.123)))
      assert(environment.getValue(Variable("C")).contains(Result("qwerty")))
      assert(environment.getValue(Variable("D")).contains(Result(false)))
    }
    Scenario("Run for loop") {
      Given("a program consisting of only for statement")
      val program=new Program(Vector(
        new Line(10,FOR(),List(Assignment(Variable("I"),Result(1)),TO(),Result(3)))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment contains looping variable")
      assert(environment.getCurrentLine.contains(10))
      assert(environment.getValue(Variable("I")).contains(Result(1)))
      assert(environment.getFor("I").contains(10))
    }
/*    Scenario("Run for loop") {
      Given("a program consisting of for loop without step")
      val program=new Program(Vector(
        new Line(10,FOR(),List(Assignment(Variable("I"),Result(1)),TO(),Result(3))),
        new Line(20,PRINT(),List(Result("A"))),
        new Line(30,NEXT(),List(Variable("I"))),
        new Line(40,PRINT(),List(Result("B")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment reflects the program")
      assert(environment.getCurrentLine.contains(40))
      assert(environment.getValue(Variable("I")).contains(Result(3)))
      assert(environment.console==List("A\n","A\n","A\n","B\n"))
    }*/
  }
}
