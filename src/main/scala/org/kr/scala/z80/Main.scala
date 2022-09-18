package org.kr.scala.z80

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.expression.{ExprNumber, ExprOperation, ExprVariable, StaticTextExpr}
import org.kr.scala.z80.program.{FOR, Line, LineNumber, NEXT, NumericAssignment, PRINT, Program, Variable}

object Main extends App {
  println("START")

  val program=new Program(Vector(
    Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(5),Some(ExprNumber(2)))),
    Line(LineNumber(20),PRINT(ExprNumber(123.456))),
    Line(LineNumber(30),NEXT()),
    Line(LineNumber(40),FOR(NumericAssignment(Variable("J"),ExprNumber(1)),ExprNumber(10),Some(ExprNumber(1.5)))),
    Line(LineNumber(50),PRINT(ExprVariable(Variable("J")))),
    Line(LineNumber(55),PRINT(StaticTextExpr("abc"))),
    Line(LineNumber(56),PRINT(ExprOperation(ExprVariable(Variable("J")),ExprNumber(10.01),"*"))),
    Line(LineNumber(60),NEXT(Variable("J")))
  ))
  program.show()
  println()

  Environment.empty
    .run(program)
    .showConsole()
    .showCurrentLine()
    .showExitCode()



  println("END")
}
