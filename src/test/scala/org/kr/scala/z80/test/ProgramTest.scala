package org.kr.scala.z80.test

import org.kr.scala.z80.{Assignment, Environment, FOR, LET, Line, LineNumber, NEXT, PRINT, Program, REM, Result, TO, Variable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ProgramTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Run program line by line") {
    Scenario("Run only REM lines") {
      Given("a program consisting of REM lines")
      val program=new Program(Vector(
        new Line(LineNumber(5),REM("comment1"),List()),
        new Line(LineNumber(15),REM("comment2"),List()),
        new Line(LineNumber(25),REM("comment3"),List()),
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment state is unchanged except for last line number")
      assert(environment.getCurrentLine.contains(LineNumber(25)))
    }
    Scenario("Run only print lines") {
      Given("a program consisting only print lines")
      val program=new Program(Vector(
        new Line(LineNumber(10),PRINT(Result("aaaa")),List()),
        new Line(LineNumber(20),PRINT(Result("bbbb")),List())
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("console contains printed output")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.console==List("aaaa\n","bbbb\n"))
    }
    Scenario("Run assignment to a variable") {
      Given("a program consisting of assignment lines")
      val program=new Program(Vector(
        new Line(LineNumber(30),LET(Assignment(Variable("A"),Result(123L))),List()),
        new Line(LineNumber(35),LET(Assignment(Variable("B"),Result(234.123))),List()),
        new Line(LineNumber(40),LET(Assignment(Variable("C"),Result("qwerty"))),List()),
        new Line(LineNumber(45),LET(Assignment(Variable("D"),Result(false))),List())
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment contains variables with expected values")
      assert(environment.getCurrentLine.contains(LineNumber(45)))
      assert(environment.getValue(Variable("A")).contains(Result(123L)))
      assert(environment.getValue(Variable("B")).contains(Result(234.123)))
      assert(environment.getValue(Variable("C")).contains(Result("qwerty")))
      assert(environment.getValue(Variable("D")).contains(Result(0)))
    }
    Scenario("Initialize for loop") {
      Given("a program consisting of only for statement")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(),List(Assignment(Variable("I"),Result(1)),TO(),Result(3)))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment contains looping variable with initial value")
      assert(environment.getCurrentLine.contains(LineNumber(10)))
      assert(environment.getValue(Variable("I")).contains(Result(1)))
      assert(environment.getFor("I").contains(LineNumber(10)))
    }
    Scenario("Run empty for loop") {
      Given("a program consisting of empty for loop without step")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(),List(Assignment(Variable("I"),Result(1)),TO(),Result(4))),
        new Line(LineNumber(20),NEXT(Variable("I")),List())
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loops is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(10)),"last line is FOR where actual condition is checked")
      assert(environment.getValue(Variable("I")).contains(Result(4)))
    }
    Scenario("Run non-empty for loop with lines after for loop") {
      Given("a program consisting of for loop without step")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(),List(Assignment(Variable("I"),Result(1)),TO(),Result(3))),
        new Line(LineNumber(20),PRINT(Result("A")),List()),
        new Line(LineNumber(30),NEXT(Variable("I")),List()),
        new Line(LineNumber(40),PRINT(Result("B")),List())
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loops is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(40)))
      assert(environment.getValue(Variable("I")).contains(Result(3)))
      assert(environment.console==List("A\n","A\n","A\n","B\n"))
    }
  }
}
