package org.kr.scala.z80

object Main extends App {
  println("START")

  val program=new Program(Vector(
    new Line(LineNumber(10),FOR(Assignment(Variable("I"),Result(1)),Result(5)/*,STEP(),Result(2)*/)),
    new Line(LineNumber(20),PRINT(Result("X"))),
    new Line(LineNumber(30),NEXT(Variable("I")))
  ))
  program.show()
  println()

  Environment.empty
    .run(program)
    .showConsole()

  println("END")
}
