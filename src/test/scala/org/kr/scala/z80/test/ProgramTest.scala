package org.kr.scala.z80.test

import org.kr.scala.z80.environment.{Environment, ExitCode, ForState, Iterator}
import org.kr.scala.z80.expression.{BlankTextExpr, ExprNumber, ExprOperation, ExprVariable, StaticTextExpr, TextExprVariable}
import org.kr.scala.z80.program.{Assignment, DATA, ExprIndex, FOR, GOSUB, GOTO, IF, LET, Line, LineNumber, NEXT, NumericAssignment, PRINT, PrintableToken, Program, READ, REM, RETURN, STOP, StatementId, Variable, VariableName}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.expression.ExprNumber._
import org.kr.scala.z80.expression.ExprVariable._

import scala.annotation.tailrec

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
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("environment state is unchanged except for last line number")
      assert(environment.getCurrentStatement.contains(StatementId(25)))
    }
    Scenario("Run only print lines") {
      Given("a program consisting only print lines")
      val program = new Program(Vector(
        Line(10, PRINT(StaticTextExpr("aaaa"))),
        Line(20, PRINT(StaticTextExpr("bbbb")))
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("console contains printed output")
      assert(environment.getCurrentStatement.contains(StatementId(20)))
      assert(environment.console.contains(List("aaaa\n", "bbbb\n")))
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
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("environment contains variables with expected values")
      assert(environment.getCurrentStatement.contains(StatementId(45)))
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
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
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
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("environment contains arrays with expected values")
      assert(environment.getValue(Variable("AR",ExprIndex.static(List(4,2)))).contains(99.88))
      assert(environment.getValue("V").contains(99.88))
      assert(environment.getValue("W").contains(99.88))
    }
    Scenario("Assign a value of and array with index being array too") {
      Given("a program consisting of assignment lines with arrays")
      And("index being an array")
      val program = new Program(Vector(
        Line(10, LET(Assignment(Variable("V",ExprIndex.static(List(1,2))),8))),
        Line(20, LET(Assignment(Variable("AR",ExprIndex(List(ExprVariable(Variable("V",ExprIndex.static(List(1,2))))))),98.99)))))
      When("program is executed")
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("environment contains arrays with expected values")
      assert(environment.getValue(Variable("AR",ExprIndex.static(List(8)))).contains(98.99))
    }
    Scenario("Print a value of a text array") {
      Given("a program consisting of assignment and print lines with test arrays")
      val program = new Program(Vector(
        Line(10, LET(Assignment(Variable("A$",ExprIndex.static(List(1))),StaticTextExpr("ABC")))),
        Line(20, PRINT(TextExprVariable(Variable("A$",ExprIndex.static(List(1))))))))
      When("program is executed")
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("console contains value of an test array")
      assert(environment.console.contains(List("ABC\n")))
    }
  }
  Feature("FOR loop") {
    Scenario("Initialize for loop") {
      Given("a program consisting of only for statement")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 1), 3))
      ))
      When("program is executed")
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("environment contains looping variable with initial value")
      assert(environment.getCurrentStatement.contains(StatementId(10)))
      assert(environment.getValue("I").contains(1))
      assert(environment.getFor("I").contains(ForState("I", 1, 3, 1, StatementId(10))))
    }
    Scenario("Run empty for loop") {
      Given("a program consisting of empty for loop without step")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 1), 4)),
        Line(20, NEXT("I"))
      ))
      When("program is executed")
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("loop is executed properly and looping variable is set to end value")
      assert(environment.getCurrentStatement.contains(StatementId(20)))
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
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("loop is executed properly and looping variable is set to end value")
      assert(environment.getCurrentStatement.contains(StatementId(40)))
      assert(environment.getValue("I").contains(3))
      assert(environment.console.contains(List("A\n", "A\n", "A\n", "B\n")))
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
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("loop is executed properly and looping variable is set to end value")
      assert(environment.getCurrentStatement.contains(StatementId(20)))
      assert(environment.getValue("I").contains(7))
      assert(environment.console.contains(List("A\n", "A\n", "A\n", "A\n", "B\n")))
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
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("loop is executed properly")
      And("looping variable is set to a value matching step but not greater than end value")
      assert(environment.getCurrentStatement.contains(StatementId(20)))
      assert(environment.getValue("I").contains(7))
      assert(environment.console.contains(List("A\n", "A\n", "A\n", "A\n", "B\n")))
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
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("loops are executed properly")
      assert(environment.getCurrentStatement.contains(StatementId(70)))
      assert(environment.getValue("I").contains(3))
      assert(environment.getValue("J").contains(2))
      assert(environment.console.contains(List(
        "X\n", "Y\n", "Y\n",
        "X\n", "Y\n", "Y\n",
        "X\n", "Y\n", "Y\n",
        "Z\n")))
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
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("both loops are executed properly")
      And("'next' and 'for' statements are correctly identified")
      assert(environment.getCurrentStatement.contains(StatementId(35)))
      assert(environment.getValue("I").contains(2))
      assert(environment.getValue("J").contains(3))
      assert(environment.console.contains(List("Q\n", "Q\n", "W\n", "W\n", "W\n", "E\n")))
    }
    Scenario("Run for loop w/o next with start value < end value") {
      Given("a program consisting of for loop w/o next")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 1), 2)),
        Line(20, PRINT(StaticTextExpr("A")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("loop is executed only once")
      And("missing next is ignored")
      assert(environment.getCurrentStatement.contains(StatementId(20)))
      assert(environment.getValue("I").contains(1))
      assert(environment.console.contains(List("A\n")))
    }
    Scenario("Run for loop w/o next with start value = end value") {
      Given("a program consisting of for loop w/o next")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 3), 3)),
        Line(20, PRINT(StaticTextExpr("A")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("loop is executed only once")
      And("missing next is ignored")
      assert(environment.getCurrentStatement.contains(StatementId(20)))
      assert(environment.getValue("I").contains(3))
      assert(environment.console.contains(List("A\n")))
    }
    Scenario("Run for loop w/o next with start value > end value") {
      Given("a program consisting of for loop w/o next")
      val program = new Program(Vector(
        Line(10, FOR(NumericAssignment("I", 2), 1)),
        Line(20, PRINT(StaticTextExpr("A")))
      ))
      When("program is executed")
      val initialEnvironment = Environment.load(program)
      val environment = initialEnvironment.run
      Then("the loop is not executed")
      And("program ends with error - missing next statement")
      assert(environment.getCurrentStatement.contains(StatementId(10)))
      assert(environment.getValue("I").contains(0))
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
      val environment = TestRunner(program).run
      Then("the loop is executed once")
      And("program ends with error - missing for statement")
      assert(environment.getCurrentStatement.contains(StatementId(30)))
      assert(environment.getValue("I").contains(1))
      assert(environment.exitCode == ExitCode.MISSING_FOR)
      assert(environment.console.contains(List("A\n")))
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
      val environment = TestRunner(program).run
      Then("jump is executed")
      assert(environment.getCurrentStatement.contains(StatementId(40)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console.contains(List("A\n", "C\n")))
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
      val environment = TestRunner(program).run
      Then("jump is not executed")
      And("program ends with error - line not found")
      assert(environment.getCurrentStatement.contains(StatementId(20)))
      assert(environment.exitCode == ExitCode.FATAL_LINE_NOT_FOUND)
      assert(environment.console.contains(List("A\n")))
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
      val environment = TestRunner(program).run
      Then("condition is checked")
      And("conditional statement is executed")
      assert(environment.getCurrentStatement.contains(StatementId(30)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console.contains(List(" 10\n", "X\n")))
    }
    Scenario("run if line with statement (fail)") {
      Given("a program consisting of if line with failing condition and statement after condition")
      val program = new Program(Vector(
        Line(10, LET(Assignment("A", 10))),
        Line(20, IF(ExprOperation.eq("A", 11), PRINT("A"))),
        Line(30, PRINT(StaticTextExpr("X")))
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("condition is evaluated to false")
      And("conditional statement is not executed")
      assert(environment.getCurrentStatement.contains(StatementId(30)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console.contains(List("X\n")))
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
      val environment = TestRunner(program).run
      Then("condition is checked")
      And("conditional statement is executed")
      assert(environment.getCurrentStatement.contains(StatementId(30)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console.contains(List("X\n")))
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
      val environment = TestRunner(program).run
      Then("condition is checked")
      And("conditional statement is executed")
      assert(environment.getCurrentStatement.contains(StatementId(30)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console.contains(List("print this line\n", "X\n")))
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
      val environment = TestRunner(program).run
      Then("jump is executed")
      assert(environment.getCurrentStatement.contains(StatementId(40)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console.contains(List("A\n", "C\n")))
      And("the program will resume after popped line")
      val envAfter = environment.popLine
      assert(envAfter.nextStatement.contains(StatementId(30)))
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
      val environment = TestRunner(program).run
      Then("jump is executed")
      assert(environment.getCurrentStatement.contains(StatementId(70)))
      assert(environment.exitCode == ExitCode.PROGRAM_END)
      assert(environment.console.contains(List("A\n", "subroutine\n", "B\n", "end\n")))
    }
    Scenario("run return without gosub") {
      Given("a program consisting of one gosub and two return lines")
      val program = new Program(Vector(
        Line(10, GOSUB(50)),
        Line(20, RETURN()),
        Line(30, STOP()),
        Line(50, RETURN())
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("return statement ends with error - return without gosub")
      assert(environment.getCurrentStatement.contains(StatementId(20)))
      assert(environment.exitCode == ExitCode.RETURN_WITHOUT_GOSUB)
    }
  }
  Feature("complex PRINT") {
    Scenario("print without tokens") {
      Given("empty print statement")
      val program = new Program(Vector(
        Line(10, PRINT(BlankTextExpr))
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("blank line is printed")
      assert(environment.console.contains(List("\n")))
    }
    Scenario("print single token") {
      Given("print statement with a single token")
      val program = new Program(Vector(
        Line(10, PRINT(StaticTextExpr("A")))
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("single token is printed")
      assert(environment.console.contains(List("A\n")))
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
      val environment = TestRunner(program).run
      Then("all tokens are printed with adequate separators (none or tab)")
      And("with EOL")
      assert(environment.console.contains(List("A\tBC\n")))
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
      val environment = TestRunner(program).run
      Then("all tokens are printed with adequate separators (none or tab)")
      And("without EOL")
      assert(environment.console.contains(List("A\tB","CD\t")))
    }
  }
  Feature("data and read statements") {
    Scenario("store data using DATA statement") {
      Given("program with data statement")
      val program = new Program(Vector(
        Line(10, DATA(List(1,2))),
        Line(20, DATA(List("ABC",5.2)))
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("data is stored")
      val data=readData(environment)
      assert(data == List(1, 2, "ABC", 5.2))
    }
    Scenario("read data using READ statement") {
      Given("program with read statement and environment with data")
      val program = new Program(Vector(
        Line(10, READ(List("A",Variable("B",ExprIndex.static(List(5)))))),
        Line(20, READ(List(Variable("C$",ExprIndex.static(List(2,3))))))
      ))
      When("program is executed")
      val initialEnvironment = Environment.load(program)
        .storeData(List(1.2,3.4,"abc"))
      val environment = initialEnvironment.run
      Then("data is read to variables")
      assert(environment.exitCode==ExitCode.PROGRAM_END)
      assert(environment.getValue("A").contains(1.2))
      assert(environment.getValue(Variable("B",ExprIndex.static(List(5)))).contains(3.4))
      assert(environment.getValue(Variable("C$",ExprIndex.static(List(2,3)))).contains("abc"))
    }
    Scenario("store and read data using DATA and  READ statements (static index)") {
      Given("program with read statement before data statements")
      val program = new Program(Vector(
        Line(10, READ(List("A",Variable("B",ExprIndex.static(List(10,2))),Variable("C$",ExprIndex.static(List(6)))))),
        Line(20, DATA(List(2.3,4.5,"QWE")))
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("data is read to variables")
      assert(environment.exitCode==ExitCode.PROGRAM_END)
      assert(environment.getValue("A").contains(2.3))
      assert(environment.getValue(Variable("B",ExprIndex.static(List(10,2)))).contains(4.5))
      assert(environment.getValue(Variable("C$",ExprIndex.static(List(6)))).contains("QWE"))
    }
    Scenario("store and read data using DATA and READ statements (variable index)") {
      Given("program with read statement before data statements")
      val program = new Program(Vector(
        Line(10, READ(
          List("I",Variable(VariableName("A"),ExprIndex(List(ExprVariable("I")))),
            Variable(VariableName("A"),ExprIndex(List(ExprOperation.plus(ExprVariable("I"),ExprNumber(1)))))))),
        Line(20, READ(List(Variable(VariableName("A"),ExprIndex(List(ExprOperation.plus(ExprVariable("I"),ExprNumber(2)))))))),
        Line(50, DATA(List(BigDecimal(1),BigDecimal(2.3),BigDecimal(4.5),BigDecimal(6.7))))
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("data is read to variables")
      assert(environment.exitCode==ExitCode.PROGRAM_END)
      assert(environment.getValue(Variable("A",ExprIndex.static(List(1)))).contains(2.3))
      assert(environment.getValue(Variable("A",ExprIndex.static(List(2)))).contains(4.5))
      assert(environment.getValue(Variable("A",ExprIndex.static(List(3)))).contains(6.7))
    }
    Scenario("read more data than available") {
      Given("program with read statement before data statements")
      val program = new Program(Vector(
        Line(10, READ(List("I"))),
        Line(20, READ(List("J"))),
        Line(30, READ(List("K"))),
        Line(40, DATA(List(BigDecimal(1),BigDecimal(2))))
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("error occurs - out of data")
      assert(environment.exitCode==ExitCode.OUT_OF_DATA)
      assert(environment.getCurrentStatement.contains(StatementId(30)))
    }
  }

  @tailrec
  private def readData(env:Environment, accumList:List[Any]=List()):List[Any] =
    env.readData match {
      case Left(_) => accumList
      case Right((newEnv,value)) => readData(newEnv,accumList :+ value)
    }

  Feature("multiple statements in one line") {
    Scenario("multiple statements in one line without jumps") {
      Given("program with multiple statements in one line")
      val program = new Program(Vector(
        Line(LineNumber(10), Vector(LET(Assignment("A", 100)),PRINT(ExprVariable("A")))),
        Line(LineNumber(20), Vector(LET(Assignment("B", 110)),PRINT(ExprVariable("B"))))))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("console contains printed output")
      assert(environment.getCurrentStatement.contains(StatementId(20,1)))
      assert(environment.console.contains(List(" 100\n"," 110\n")))
      assert(environment.getValue("A").contains(100))
      assert(environment.getValue("B").contains(110))
    }
    Scenario("multiple statements in one line with for loop") {
      Given("program with multiple statements in one line and for loop")
      val program = new Program(Vector(
        Line(LineNumber(10), Vector(PRINT(StaticTextExpr("START")),FOR(NumericAssignment("I",ExprNumber(1)),ExprNumber(2)))),
        Line(LineNumber(20), Vector(PRINT("I"),NEXT("I"),PRINT(StaticTextExpr("END"))))))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("console contains printed output")
      assert(environment.getCurrentStatement.contains(StatementId(20,2)))
      assert(environment.console.contains(List("START\n"," 1\n"," 2\n","END\n")))
    }
    Scenario("multiple statements in one line with gosub") {
      Given("program with multiple statements in one line and gosub jumps")
      val program = new Program(Vector(
        Line(LineNumber(10), Vector(PRINT(1),GOSUB(100),GOSUB(200),GOSUB(300),GOTO(1000))),
        Line(LineNumber(100), Vector(PRINT(2),RETURN())),
        Line(LineNumber(200), Vector(PRINT(3),RETURN())),
        Line(LineNumber(300), Vector(PRINT(4),RETURN())),
        Line(LineNumber(1000), Vector(PRINT(5)))))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("console contains printed output")
      assert(environment.getCurrentStatement.contains(StatementId(1000)))
      assert(environment.console.contains(List(" 1\n"," 2\n"," 3\n"," 4\n"," 5\n")))
    }
  }
  Feature("stop program") {
    Scenario("stop program") {
      Given("program with stop statement")
      val program = new Program(Vector(
        Line(LineNumber(10), Vector(PRINT(StaticTextExpr("A")))),
        Line(LineNumber(20), Vector(PRINT(StaticTextExpr("B")),STOP(),PRINT(StaticTextExpr("C")))),
        Line(LineNumber(30), Vector(PRINT(StaticTextExpr("D")))),
      ))
      When("program is executed")
      val environment = TestRunner(program).run
      Then("program ends at line with stop statement")
      assert(environment.getCurrentStatement.contains(StatementId(20,1)))
      assert(environment.exitCode==ExitCode.PROGRAM_END)
      assert(environment.console.contains(List("A\n","B\n")))
    }
  }
}

case class TestRunner(program: Program) {
  def run:Environment = Iterator[Environment](Environment.finishByCode).iterate(Environment.load(program))
}