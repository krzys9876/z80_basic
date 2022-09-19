package org.kr.scala.z80.test

import org.kr.scala.z80.environment.{Environment, ExitCode, ForState}
import org.kr.scala.z80.expression.{ExprNumber, StaticTextExpr}
import org.kr.scala.z80.program.{Assignment, FOR, GOTO, LET, Line, LineNumber, NEXT, NumericAssignment, PRINT, Program, REM, Variable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.expression.ExprNumber._

class ProgramTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Run dummy program line by line") {
    Scenario("Run only REM lines") {
      Given("a program consisting of REM lines")
      val program = new Program(Vector(
        Line(5, REM("comment1")),
        Line(15, REM("comment2")),
        Line(25, REM("comment3")),
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("environment state is unchanged except for last line number")
      assert(environment.getCurrentLine.contains(LineNumber(25)))
    }
    Scenario("Run only print lines") {
      Given("a program consisting only print lines")
      val program = new Program(Vector(
        Line(10, PRINT(StaticTextExpr("aaaa"))),
        Line(20, PRINT(StaticTextExpr("bbbb")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("console contains printed output")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.console == List("aaaa\n", "bbbb\n"))
    }
  }
  Feature("assignment to variable") {
    Scenario("Run assignment to a variable") {
      Given("a program consisting of assignment lines")
      val program = new Program(Vector(
        Line(30, LET(Assignment("A", 123L))),
        Line(35, LET(Assignment("B", 234.123))),
        Line(40, LET(Assignment("C", StaticTextExpr("qwerty")))),
        Line(45, LET(Assignment("D", ExprNumber(false))))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("environment contains variables with expected values")
      assert(environment.getCurrentLine.contains(LineNumber(45)))
      assert(environment.getValue(Variable("A")).contains(123.0))
      assert(environment.getValue(Variable("B")).contains(234.123))
      assert(environment.getValue(Variable("C")).contains("qwerty"))
      assert(environment.getValue(Variable("D")).contains(0))
    }
    Scenario("Initialize for loop") {
      Given("a program consisting of only for statement")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 1), 3))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("environment contains looping variable with initial value")
      assert(environment.getCurrentLine.contains(LineNumber(10)))
      assert(environment.getValue("I").contains(1))
      assert(environment.getFor("I").contains(ForState("I", 1, 3, 1, 10)))
    }
  }
  Feature("FOR loop") {
    Scenario("Run empty for loop") {
      Given("a program consisting of empty for loop without step")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 1), 4)),
        Line(20, NEXT("I"))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("loop is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue("I").contains(4))
    }
    Scenario("Run non-empty for loop with lines after for loop") {
      Given("a program consisting of for loop without step")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 1), 3)),
        Line(20, PRINT(StaticTextExpr("A"))),
        Line(30, NEXT("I")),
        Line(40, PRINT(StaticTextExpr("B")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("loop is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(40)))
      assert(environment.getValue("I").contains(3))
      assert(environment.console == List("A\n", "A\n", "A\n", "B\n"))
    }
    Scenario("Run non-empty for loop with lines after for loop and step") {
      Given("a program consisting of for loop with step")
      val program = new Program(Vector(
        Line(5, FOR(NumericAssignment("I", 1), 7, Some(2))),
        Line(10, PRINT(StaticTextExpr("A"))),
        Line(15, NEXT("I")),
        Line(20, PRINT(StaticTextExpr("B")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("loop is executed properly and looping variable is set to end value")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue("I").contains(7))
      assert(environment.console == List("A\n", "A\n", "A\n", "A\n", "B\n"))
    }
    Scenario("Run non-empty for loop with lines after for loop and step not matching end value") {
      Given("a program consisting of for loop with step")
      val program = new Program(Vector(
        Line(5, FOR(NumericAssignment("I", 1), 8, Some(2))),
        Line(10, PRINT(StaticTextExpr("A"))),
        Line(15, NEXT("I")),
        Line(20, PRINT(StaticTextExpr("B")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("loop is executed properly")
      And("looping variable is set to a value matching step but not greater than end value")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue("I").contains(7))
      assert(environment.console == List("A\n", "A\n", "A\n", "A\n", "B\n"))
    }
    Scenario("Run nested for loops") {
      Given("a program consisting of nested for loops")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 1), 3)),
        Line(20, PRINT(StaticTextExpr("X"))),
        Line(30, FOR(NumericAssignment("J", 1), 2)),
        Line(40, PRINT(StaticTextExpr("Y"))),
        Line(50, NEXT("J")),
        Line(60, NEXT("I")),
        Line(70, PRINT(StaticTextExpr("Z")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("loops are executed properly")
      assert(environment.getCurrentLine.contains(LineNumber(70)))
      assert(environment.getValue(Variable("I")).contains(3))
      assert(environment.getValue(Variable("J")).contains(2))
      assert(environment.console == List(
        "X\n", "Y\n", "Y\n",
        "X\n", "Y\n", "Y\n",
        "X\n", "Y\n", "Y\n",
        "Z\n"))
    }
    Scenario("Run non-empty for loop w/o variable after next") {
      Given("a program consisting of for loops")
      val program = new Program(Vector(
        Line(5, FOR(NumericAssignment("I", 1), 2)),
        Line(10, PRINT(StaticTextExpr("Q"))),
        Line(15, NEXT()),
        Line(20, FOR(NumericAssignment("J", 1), 3)),
        Line(25, PRINT(StaticTextExpr("W"))),
        Line(30, NEXT()),
        Line(35, PRINT(StaticTextExpr("E")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("both loops are executed properly")
      And("'next' and 'for' statements are correctly identified")
      assert(environment.getCurrentLine.contains(LineNumber(35)))
      assert(environment.getValue("I").contains(2))
      assert(environment.getValue("J").contains(3))
      assert(environment.console == List("Q\n", "Q\n", "W\n", "W\n", "W\n", "E\n"))
    }
    Scenario("Run for loop w/o next with start value < end value") {
      Given("a program consisting of for loop w/o next")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 1), 2)),
        Line(20, PRINT(StaticTextExpr("A")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("loop is executed only once")
      And("missing next is ignored")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue("I").contains(1))
      assert(environment.console == List("A\n"))
    }
    Scenario("Run for loop w/o next with start value = end value") {
      Given("a program consisting of for loop w/o next")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 3), 3)),
        Line(20, PRINT(StaticTextExpr("A")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("loop is executed only once")
      And("missing next is ignored")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.getValue("I").contains(3))
      assert(environment.console == List("A\n"))
    }
    Scenario("Run for loop w/o next with start value > end value") {
      Given("a program consisting of for loop w/o next")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 2), 1)),
        Line(20, PRINT(StaticTextExpr("A")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("the loop is not executed")
      And("program ends with error - missing next statement")
      assert(environment.getCurrentLine.contains(LineNumber(10)))
      assert(environment.getValue("I").isEmpty)
      assert(environment.exitCode == ExitCode.MISSING_NEXT)
      assert(environment.console.isEmpty)
    }
    Scenario("Run for loop with incorrect next") {
      Given("a program consisting of for loop with incorrect next (different variable)")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 1), 2)),
        Line(20, PRINT(StaticTextExpr("A"))),
        Line(30, NEXT("J"))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("the loop is executed once")
      And("program ends with error - missing for statement")
      assert(environment.getCurrentLine.contains(LineNumber(30)))
      assert(environment.getValue("I").contains(1))
      assert(environment.exitCode == ExitCode.MISSING_FOR)
      assert(environment.console == List("A\n"))
    }
  }
  Feature("GOTO jump") {
    Scenario("run goto line") {
      Given("a program consisting of goto line and other lines")
      val program = new Program(Vector(
        Line(10, PRINT(StaticTextExpr("A"))),
        Line(20, GOTO(40)),
        Line(30, PRINT(StaticTextExpr("B"))),
        Line(40, PRINT(StaticTextExpr("C"))),
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("jump is executed")
      assert(environment.getCurrentLine.contains(LineNumber(40)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console == List("A\n","C\n"))
    }
    Scenario("run goto non-existing line") {
      Given("a program consisting of goto line and other lines")
      val program = new Program(Vector(
        Line(10, PRINT(StaticTextExpr("A"))),
        Line(20, GOTO(25)),
        Line(30, PRINT(StaticTextExpr("B"))),
        Line(40, PRINT(StaticTextExpr("C"))),
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("jump is not executed")
      And("program ends with error - line not found")
      assert(environment.getCurrentLine.contains(LineNumber(20)))
      assert(environment.exitCode == ExitCode.FATAL_LINE_NOT_FOUND)
      assert(environment.console == List("A\n"))
    }
  }
}
