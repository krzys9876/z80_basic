package org.kr.scala.z80

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.expression.{ExprNumber, ExprOperation, ExprVariable}
import org.kr.scala.z80.program.parser.LineParser
import org.kr.scala.z80.program.{FOR, Line, LineNumber, NumericAssignment, PRINT, Program, Variable}

object Main extends App {
  println("START")

  val program=new Program(Vector(
    Line(LineNumber(10),FOR(NumericAssignment(Variable("I"),ExprNumber(1)),ExprNumber(5),Some(ExprNumber(2)))),
    LineParser.force("20 PRINT 123.456"),
    LineParser.force("30 NEXT"),
    Line(LineNumber(40),FOR(NumericAssignment(Variable("J"),ExprNumber(1)),ExprNumber(10),Some(ExprNumber(1.5)))),
    LineParser.force("50 PRINT J"),
    LineParser.force("55 PRINT \"abc\""),
    LineParser.force("56 PRINT J*10.01"),
    LineParser.force("60 NEXT J")
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
