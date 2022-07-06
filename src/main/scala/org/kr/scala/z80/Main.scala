package org.kr.scala.z80

object Main extends App {
  println("START")

  val program=new Program(Vector(
    new Line(10,FOR(),List(Assignment(Variable("I"),Result(1)),TO(),Result(11),STEP(),Result(2))),
    new Line(20,PRINT(),List(Variable("I"))),
    new Line(30,NEXT(),List(Variable("I")))
  ))

  program.show()

  println("END")
}
