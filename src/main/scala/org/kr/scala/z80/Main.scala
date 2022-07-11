package org.kr.scala.z80

object Main extends App {
  println("START")

  val program=new Program(Vector(
    /*new Line(LineNumber(10),FOR(Assignment(Variable("I"),Result(1)),Result(5),Some(Result(2)))),
    new Line(LineNumber(20),PRINT(Result("X"))),
    new Line(LineNumber(30),NEXT()),*/
    new Line(LineNumber(40),FOR(Assignment(Variable("I"),Result(1)),Result(5),Some(Result(2)))),
    new Line(LineNumber(50),PRINT(Result("Y"))),
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
