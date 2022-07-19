package org.kr.scala.z80

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.expression.{ExprNumber, ExprVariable, StaticTextExpr}
import org.kr.scala.z80.program.{FOR, Line, LineNumber, NEXT, NumericAssignment, PRINT, Program, Variable}

object Main extends App {
  println("START")

  val program=new Program(Vector(
    new Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(5),Some(ExprNumber(2)))),
    new Line(LineNumber(20),PRINT(ExprNumber(123.456))),
    new Line(LineNumber(30),NEXT()),
    new Line(LineNumber(40),FOR(NumericAssignment(Variable("J"),ExprNumber(1)),ExprNumber(10),Some(ExprNumber(1.5)))),
    new Line(LineNumber(50),PRINT(ExprVariable(Variable("J")))),
    new Line(LineNumber(55),PRINT(StaticTextExpr("abc"))),
    new Line(LineNumber(60),NEXT(Variable("J")))
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
