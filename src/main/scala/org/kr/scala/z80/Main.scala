package org.kr.scala.z80

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.program.parser.LineParser
import org.kr.scala.z80.program.Program

object Main extends App {
  println("START")

  val program=new Program(Vector(
    LineParser.force("10 FOR I=1 TO 5 STEP 2"),
    LineParser.force("20 PRINT (123.456+I)^2"),
    LineParser.force("30 NEXT"),
    LineParser.force("40 FOR J=1 TO 10 STEP 1.5"),
    LineParser.force("50 PRINT J"),
    LineParser.force("52 IF J>1 THEN GOTO 55"),
    LineParser.force("53 PRINT \"first iteration\""),
    LineParser.force("55 IF J=10 THEN PRINT \"last iteration\""),
    LineParser.force("56 PRINT J*10.01"),
    LineParser.force("60 NEXT J"),
    LineParser.force("70 GOTO 80"),
    LineParser.force("75 PRINT \"this line should be skipped\""),
    LineParser.force("80 PRINT \"program ends here\""),
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
