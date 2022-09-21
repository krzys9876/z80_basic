package org.kr.scala.z80.test

import org.kr.scala.z80.environment.{Environment, ExitCode, ForState}
import org.kr.scala.z80.expression.{BlankTextExpr, ExprNumber, ExprOperation, ExprVariable, StaticTextExpr}
import org.kr.scala.z80.program.{Assignment, ExprIndex, FOR, GOSUB, GOTO, IF, Index, LET, Line, LineNumber, NEXT, NumericAssignment, PRINT, PrintableToken, Program, REM, RETURN, VariableName, Variable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.expression.ExprNumber._
import org.kr.scala.z80.expression.ExprVariable._

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
      assert(environment.getValue("A").contains(123.0))
      assert(environment.getValue("B").contains(234.123))
      assert(environment.getValue("C").contains("qwerty"))
      assert(environment.getValue("D").contains(0))
    }
    Scenario("Run assignment to an array") {
      Given("a program consisting of assignment lines with arrays")
      val program = new Program(Vector(
        Line(30, LET(Assignment(Variable("A",ExprIndex.static(List(1,2))), 100))),
        Line(31, LET(Assignment(Variable("A",ExprIndex.static(List(1,3))), 200))),
        Line(32, LET(Assignment(Variable("A",ExprIndex.static(List(1,2))), 101))), // the same index!
        Line(33, LET(Assignment(Variable("B",ExprIndex.static(List(1))), 10.12))),
        Line(34, LET(Assignment(Variable("B",ExprIndex.static(List(2))), 20.22))),
        Line(35, LET(Assignment(Variable("B",ExprIndex.static(List(1))), 11.12))), // the same index!
        //NOTE: due to implicit conversion from string to Variable a string value must be StaticTextExpr, not just a string
        Line(36, LET(Assignment(Variable("C$",ExprIndex.static(List(1,2,3,4))), StaticTextExpr("ABC")))),
        Line(37, LET(Assignment(Variable("C$",ExprIndex.static(List(4,5,6,7))), StaticTextExpr("XYZ")))),
        Line(38, LET(Assignment(Variable("C$",ExprIndex.static(List(1,2,3,4))), StaticTextExpr("abc")))), // the same index!
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("environment contains arrays with expected values")
      assert(environment.getValue(Variable("A",ExprIndex.static(List(1,2)))).contains(101))
      assert(environment.getValue(Variable("A",ExprIndex.static(List(1,3)))).contains(200))
      assert(environment.getValue(Variable("B",ExprIndex.static(List(1)))).contains(11.12))
      assert(environment.getValue(Variable("B",ExprIndex.static(List(2)))).contains(20.22))
      assert(environment.getValue(Variable("C$",ExprIndex.static(List(1,2,3,4)))).contains("abc"))
      assert(environment.getValue(Variable("C$",ExprIndex.static(List(4,5,6,7)))).contains("XYZ"))
    }
    Scenario("Assign a value of and array to a variable") {
      Given("a program consisting of assignment lines with arrays")
      val program = new Program(Vector(
        Line(10, LET(Assignment("V",12.34))),
        Line(20, LET(Assignment(Variable("AR",ExprIndex.static(List(4,2))), 99.88))),
        Line(30, LET(Assignment("V",ExprVariable(Variable("AR",ExprIndex.static(List(4,2))))))),
        Line(40, LET(Assignment("W",ExprVariable(Variable("AR",ExprIndex.static(List(4,2)))))))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("environment contains arrays with expected values")
      assert(environment.getValue(Variable("AR",ExprIndex.static(List(4,2)))).contains(99.88))
      assert(environment.getValue("V").contains(99.88))
      assert(environment.getValue("W").contains(99.88))
    }
  }
  Feature("FOR loop") {
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
      assert(environment.getValue("I").contains(3))
      assert(environment.getValue("J").contains(2))
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
      assert(environment.getValue("I").isLeft)
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
      assert(environment.console == List("A\n", "C\n"))
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
  Feature("IF statement") {
    Scenario("run if line with statement (pass)") {
      Given("a program consisting of if line with passing condition and statement after condition")
      val program = new Program(Vector(
        Line(10, LET(Assignment("A", 10))),
        Line(20, IF(ExprOperation.eq("A", 10), PRINT("A"))),
        Line(30, PRINT(StaticTextExpr("X")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("condition is checked")
      And("conditional statement is executed")
      assert(environment.getCurrentLine.contains(LineNumber(30)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console == List("10\n", "X\n"))
    }
    Scenario("run if line with statement (fail)") {
      Given("a program consisting of if line with failing condition and statement after condition")
      val program = new Program(Vector(
        Line(10, LET(Assignment("A", 10))),
        Line(20, IF(ExprOperation.eq("A", 11), PRINT("A"))),
        Line(30, PRINT(StaticTextExpr("X")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("condition is evaluated to false")
      And("conditional statement is not executed")
      assert(environment.getCurrentLine.contains(LineNumber(30)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console == List("X\n"))
    }
    Scenario("run if line with jump (pass)") {
      Given("a program consisting of if line with passing condition and jump (goto)")
      val program = new Program(Vector(
        Line(10, LET(Assignment("A", 10))),
        Line(20, IF(ExprOperation.eq("A", 10), GOTO(30))),
        Line(25, PRINT(StaticTextExpr("skip this line"))),
        Line(30, PRINT(StaticTextExpr("X")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("condition is checked")
      And("conditional statement is executed")
      assert(environment.getCurrentLine.contains(LineNumber(30)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console == List("X\n"))
    }
    Scenario("run if line with jump (fail)") {
      Given("a program consisting of if line with failing condition and jump (goto)")
      val program = new Program(Vector(
        Line(10, LET(Assignment("B", 10))),
        Line(20, IF(ExprOperation.eq("B", 9), GOTO(30))),
        Line(25, PRINT(StaticTextExpr("print this line"))),
        Line(30, PRINT(StaticTextExpr("X")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("condition is checked")
      And("conditional statement is executed")
      assert(environment.getCurrentLine.contains(LineNumber(30)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console == List("print this line\n", "X\n"))
    }
  }
  Feature("GOSUB jump") {
    Scenario("run gosub line") {
      Given("a program consisting of gosub line and other lines")
      val program = new Program(Vector(
        Line(10, PRINT(StaticTextExpr("A"))),
        Line(20, GOSUB(40)),
        Line(30, PRINT(StaticTextExpr("B"))),
        Line(40, PRINT(StaticTextExpr("C"))),
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("jump is executed")
      assert(environment.getCurrentLine.contains(LineNumber(40)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console == List("A\n", "C\n"))
      And("the program will resume after popped line")
      val envAfter = environment.popLine(program)
      assert(envAfter.nextLineNum.contains(LineNumber(30)))
    }
    Scenario("run gosub and return") {
      Given("a program consisting of gosub, return and other lines")
      val program = new Program(Vector(
        Line(10, PRINT(StaticTextExpr("A"))),
        Line(20, GOSUB(50)),
        Line(30, PRINT(StaticTextExpr("B"))),
        Line(40, GOTO(70)),
        Line(50, PRINT(StaticTextExpr("subroutine"))),
        Line(60, RETURN()),
        Line(70, PRINT(StaticTextExpr("end"))),
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("jump is executed")
      assert(environment.getCurrentLine.contains(LineNumber(70)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console == List("A\n", "subroutine\n", "B\n", "end\n"))
    }
  }
  Feature("complex PRINT") {
    Scenario("print without tokens") {
      Given("empty print statement")
      val program = new Program(Vector(
        Line(10, PRINT(BlankTextExpr))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("blank line is printed")
      assert(environment.console == List("\n"))
    }
    Scenario("print single token") {
      Given("print statement with a single token")
      val program = new Program(Vector(
        Line(10, PRINT(StaticTextExpr("A")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("single token is printed")
      assert(environment.console == List("A\n"))
    }
    Scenario("print multiple tokens separated with ; or , with EOP") {
      Given("print statements with multiple tokens separated with ; or ,")
      And("not ended with ;")
      val program = new Program(Vector(
        Line(10, PRINT(Vector(
          PrintableToken(StaticTextExpr("A")),
          PrintableToken(Some(","),StaticTextExpr("B")),
          PrintableToken(Some(";"),StaticTextExpr("C"))))
      )))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("all tokens are printed with adequate separators (none or tab)")
      And("with EOL")
      assert(environment.console == List("A\tBC\n"))
    }
    Scenario("print multiple tokens token ; without EOP") {
      Given("print statements with multiple tokens separated with ;")
      And("ended with ;")
      val program = new Program(Vector(
        Line(10, PRINT(Vector(
          PrintableToken(StaticTextExpr("A")),
          PrintableToken(Some(","),StaticTextExpr("B")),
          PrintableToken(Some(";"),BlankTextExpr)))),
        Line(20, PRINT(Vector(
          PrintableToken(StaticTextExpr("C")),
          PrintableToken(Some(";"),StaticTextExpr("D")),
          PrintableToken(Some(","),BlankTextExpr)))
        )))
      When("program is executed")
      val initialEnvironment = Environment.empty
      val environment = initialEnvironment.run(program)
      Then("all tokens are printed with adequate separators (none or tab)")
      And("without EOL")
      assert(environment.console == List("A\tB","CD\t"))
    }
  }
}
