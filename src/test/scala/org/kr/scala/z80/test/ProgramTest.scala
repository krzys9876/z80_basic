package org.kr.scala.z80.test

import org.kr.scala.z80.{Assignment, Environment, ExitCode, ExprNumber, FOR, ForState, LET, Line, LineNumber, NEXT, NumericAssignment, PRINT, Program, REM, Result, Variable}
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
        new Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(3)))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("environment contains looping variable with initial value")
      assert(environment.getCurrentLine.contains(LineNumber(10)))
      assert(environment.getValue(Variable("I")).contains(1))
      assert(environment.getFor(Variable("I")).contains(ForState(Variable("I"),LineNumber(10))))
    }
    Scenario("Run empty for loop") {
      Given("a program consisting of empty for loop without step")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(4))),
        new Line(LineNumber(20),NEXT(Variable("I")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loop is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue(Variable("I")).contains(4))
    }
    Scenario("Run non-empty for loop with lines after for loop") {
      Given("a program consisting of for loop without step")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(3))),
        new Line(LineNumber(20),PRINT(Result("A"))),
        new Line(LineNumber(30),NEXT(Variable("I"))),
        new Line(LineNumber(40),PRINT(Result("B")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loop is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(40)))
      assert(environment.getValue(Variable("I")).contains(3))
      assert(environment.console==List("A\n","A\n","A\n","B\n"))
    }
    Scenario("Run non-empty for loop with lines after for loop and step") {
      Given("a program consisting of for loop with step")
      val program=new Program(Vector(
        new Line(LineNumber(5),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(7),Some(ExprNumber(2)))),
        new Line(LineNumber(10),PRINT(Result("A"))),
        new Line(LineNumber(15),NEXT(Variable("I"))),
        new Line(LineNumber(20),PRINT(Result("B")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loop is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue(Variable("I")).contains(7))
      assert(environment.console==List("A\n","A\n","A\n","A\n","B\n"))
    }
    Scenario("Run non-empty for loop with lines after for loop and step not matching end value") {
      Given("a program consisting of for loop with step")
      val program=new Program(Vector(
        new Line(LineNumber(5),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(8),Some(ExprNumber(2)))),
        new Line(LineNumber(10),PRINT(Result("A"))),
        new Line(LineNumber(15),NEXT(Variable("I"))),
        new Line(LineNumber(20),PRINT(Result("B")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loop is executed properly")
      And("looping variable is set to a value matching step but not greater than end value")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue(Variable("I")).contains(7))
      assert(environment.console==List("A\n","A\n","A\n","A\n","B\n"))
    }
    Scenario("Run nested for loops") {
      Given("a program consisting of nested for loops")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(3))),
        new Line(LineNumber(20),PRINT(Result("X"))),
        new Line(LineNumber(30),FOR(NumericAssignment(Variable("J"),ExprNumber(1)),ExprNumber(2))),
        new Line(LineNumber(40),PRINT(Result("Y"))),
        new Line(LineNumber(50),NEXT(Variable("J"))),
        new Line(LineNumber(60),NEXT(Variable("I"))),
        new Line(LineNumber(70),PRINT(Result("Z")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loops are executed properly")
      assert(environment.getCurrentLine.contains(LineNumber(70)))
      assert(environment.getValue(Variable("I")).contains(3))
      assert(environment.getValue(Variable("J")).contains(2))
      assert(environment.console==List(
        "X\n","Y\n","Y\n",
        "X\n","Y\n","Y\n",
        "X\n","Y\n","Y\n",
        "Z\n"))
    }
    Scenario("Run non-empty for loop w/o variable after next") {
      Given("a program consisting of for loops")
      val program=new Program(Vector(
        new Line(LineNumber(5),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(2))),
        new Line(LineNumber(10),PRINT(Result("Q"))),
        new Line(LineNumber(15),NEXT()),
        new Line(LineNumber(20),FOR(NumericAssignment(Variable("J"),ExprNumber(1)),ExprNumber(3))),
        new Line(LineNumber(25),PRINT(Result("W"))),
        new Line(LineNumber(30),NEXT()),
        new Line(LineNumber(35),PRINT(Result("E")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("both loops are executed properly")
      And("'next' and 'for' statements are correctly identified")
      assert(environment.getCurrentLine.contains(LineNumber(35)))
      assert(environment.getValue(Variable("I")).contains(2))
      assert(environment.getValue(Variable("J")).contains(3))
      assert(environment.console==List("Q\n","Q\n","W\n","W\n","W\n","E\n"))
    }
    Scenario("Run for loop w/o next with start value < end value") {
      Given("a program consisting of for loop w/o next")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(2))),
        new Line(LineNumber(20),PRINT(Result("A")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loop is executed only once")
      And("missing next is ignored")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue(Variable("I")).contains(1))
      assert(environment.console==List("A\n"))
    }
    Scenario("Run for loop w/o next with start value = end value") {
      Given("a program consisting of for loop w/o next")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(3)),ExprNumber(3))),
        new Line(LineNumber(20),PRINT(Result("A")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("loop is executed only once")
      And("missing next is ignored")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue(Variable("I")).contains(3))
      assert(environment.console==List("A\n"))
    }
    Scenario("Run for loop w/o next with start value > end value") {
      Given("a program consisting of for loop w/o next")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(2)),ExprNumber(1))),
        new Line(LineNumber(20),PRINT(Result("A")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("the loop is not executed")
      And("program ends with error - missing next statement")
      assert(environment.getCurrentLine.contains(LineNumber(10)))
      assert(environment.getValue(Variable("I")).isEmpty)
      assert(environment.exitCode==ExitCode.MISSING_NEXT)
      assert(environment.console.isEmpty)
    }
    Scenario("Run for loop with incorrect next") {
      Given("a program consisting of for loop with incorrect next (different variable)")
      val program=new Program(Vector(
        new Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(2))),
        new Line(LineNumber(20),PRINT(Result("A"))),
        new Line(LineNumber(30),NEXT(Variable("J")))
      ))
      When("program is executed")
      val initialEnvironment=Environment.empty
      val environment=initialEnvironment.run(program)
      Then("the loop is executed once")
      And("program ends with error - missing for statement")
      assert(environment.getCurrentLine.contains(LineNumber(30)))
      assert(environment.getValue(Variable("I")).contains(1))
      assert(environment.exitCode==ExitCode.MISSING_FOR)
      assert(environment.console==List("A\n"))
    }
  }
}
