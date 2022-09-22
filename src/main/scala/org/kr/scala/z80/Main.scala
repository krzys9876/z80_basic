package org.kr.scala.z80

import org.kr.scala.z80.environment.Environment
import org.kr.scala.z80.parser.LineParser
import org.kr.scala.z80.program.Program

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Main extends App {
  println("START")

  val program=args.length match {
    case 0 =>
      println("No arguments found, using dummy program.")
      DummyProgram.program
    case 1 =>
      val inputFile=Files.readAllLines(Path.of(args(0))).asScala.toVector
      new Program(inputFile.map(l=>{
        println(f"parsing: $l")
        LineParser.force(l)
      }))
  }

  //TODO: READ multiple variables separated with comma
  //TODO: variable index as value of an array
  //TODO: implement STOP

  program.show()
  println()

  Environment.empty
    .run(program)
    .showCurrentLine()
    .showExitCode()

  println("END")
}

object DummyProgram {
  val program= new Program(Vector(
    LineParser.force("5 REM assign some values to an array"),
    LineParser.force("8 DIM A(25)"),
    LineParser.force("10 FOR I=1 TO 5"),
    LineParser.force("20 READ A(20+I)"),
    LineParser.force("30 NEXT"),
    LineParser.force("35 REM print values in a subroutine"),
    LineParser.force("40 FOR J=1 TO 10 STEP 1.5"),
    LineParser.force("50 GOSUB 200"),
    LineParser.force("60 NEXT J"),
    LineParser.force("65 REM print the array in a subroutine"),
    LineParser.force("70 GOSUB 300"),
    LineParser.force("99 REM jump to end"),
    LineParser.force("100 GOTO 1000"),
    LineParser.force("120 PRINT \"this line should be skipped\""),
    LineParser.force("200 PRINT J,J*10.01;"),
    LineParser.force("210 IF J>1 THEN GOTO 230"),
    LineParser.force("220 PRINT \" first\";"),
    LineParser.force("230 IF J=10 THEN PRINT \" last\";"),
    LineParser.force("240 PRINT"),
    LineParser.force("250 RETURN"),
    LineParser.force("300 FOR K=21 TO 25"),
    LineParser.force("310 V=A(K)"),
    LineParser.force("320 PRINT K-20;\":\";V;\"|\";"),
    LineParser.force("330 NEXT"),
    LineParser.force("340 PRINT"),
    LineParser.force("350 RETURN"),
    LineParser.force("500 DATA 11,12,13,14,15"),
    LineParser.force("1000 PRINT \"the program ends here\""),
  ))
}