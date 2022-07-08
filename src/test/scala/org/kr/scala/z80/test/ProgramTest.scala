package org.kr.scala.z80.test

import org.kr.scala.z80.{Assignment, Environment, FOR, LET, Line, LineNumber, NEXT, PRINT, Program, REM, Result, Variable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ProgramTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Run program line by line") {
    Scenario("Run only REM lines") {
      Given("a program consisting of REM lines")
      val program=new Program(Vector(
        new Line(LineNumber(5),REM("comment1")),
        new Line(LineNumber(15),REM("comment2")),
        new Line(LineNumber(25),REM("comment3")),
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
        new Line(LineNumber(10),PRINT(Result("aaaa"))),
        new Line(LineNumber(20),PRINT(Result("bbbb")))
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
        new Line(LineNumber(30),LET(Assignment(Variable("A"),Result(123L)))),
        new Line(LineNumber(35),LET(Assignment(Variable("B"),Result(234.123)))),
        new Line(LineNumber(40),LET(Assignment(Variable("C"),Result("qwerty")))),
        new Line(LineNumber(45),LET(Assignment(Variable("D"),Result(false))))
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
        new Line(LineNumber(10),FOR(Assignment(Variable("I"),Result(1)),Result(3)))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment contains looping variable with initial value")
      assert(environment.getCurrentLine.contains(LineNumber(10)))
      assert(environment.getValue(Variable("I")).contains(Result(1)))
      assert(environment.getFor(Left("I")).contains(LineNumber(10)))
    }
    Scenario("Run empty for loop") {
      Given("a program consisting of empty for loop without step")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(Assignment(Variable("I"),Result(1)),Result(4))),
        new Line(LineNumber(20),NEXT(Variable("I")))
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
        new Line(LineNumber(10),FOR(Assignment(Variable("I"),Result(1)),Result(3))),
        new Line(LineNumber(20),PRINT(Result("A"))),
        new Line(LineNumber(30),NEXT(Variable("I"))),
        new Line(LineNumber(40),PRINT(Result("B")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loops is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(40)))
      assert(environment.getValue(Variable("I")).contains(Result(3)))
      assert(environment.console==List("A\n","A\n","A\n","B\n"))
    }
    Scenario("Run non-empty for loop with lines after for loop and step") {
      Given("a program consisting of for loop without step")
      val program=new Program(Vector(
        new Line(LineNumber(5),FOR(Assignment(Variable("I"),Result(1)),Result(7),Some(Result(2)))),
        new Line(LineNumber(10),PRINT(Result("A"))),
        new Line(LineNumber(15),NEXT(Variable("I"))),
        new Line(LineNumber(20),PRINT(Result("B")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loops is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue(Variable("I")).contains(Result(7)))
      assert(environment.console==List("A\n","A\n","A\n","A\n","B\n"))
    }
    Scenario("Run non-empty for loop with lines after for loop and step not matching end value") {
      Given("a program consisting of for loop without step")
      val program=new Program(Vector(
        new Line(LineNumber(5),FOR(Assignment(Variable("I"),Result(1)),Result(8),Some(Result(2)))),
        new Line(LineNumber(10),PRINT(Result("A"))),
        new Line(LineNumber(15),NEXT(Variable("I"))),
        new Line(LineNumber(20),PRINT(Result("B")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loops is executed properly")
      And("looping variable is set to a value matching step bit not greater than end value")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue(Variable("I")).contains(Result(7)))
      assert(environment.console==List("A\n","A\n","A\n","A\n","B\n"))
    }
    Scenario("Run nested for loops") {
      Given("a program consisting of nested for loops")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(Assignment(Variable("I"),Result(1)),Result(3))),
        new Line(LineNumber(20),PRINT(Result("X"))),
        new Line(LineNumber(30),FOR(Assignment(Variable("J"),Result(1)),Result(2))),
        new Line(LineNumber(40),PRINT(Result("Y"))),
        new Line(LineNumber(50),NEXT(Variable("J"))),
        new Line(LineNumber(60),NEXT(Variable("I"))),
        new Line(LineNumber(70),PRINT(Result("Z")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loops is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(70)))
      assert(environment.getValue(Variable("I")).contains(Result(3)))
      assert(environment.getValue(Variable("J")).contains(Result(2)))
      assert(environment.console==List(
        "X\n","Y\n","Y\n",
        "X\n","Y\n","Y\n",
        "X\n","Y\n","Y\n",
        "Z\n"))
    }
  }
}
