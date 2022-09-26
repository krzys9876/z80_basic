package org.kr.scala.z80

import org.kr.scala.z80.environment.{Environment, Iterator}
import org.kr.scala.z80.program.Program
import org.kr.scala.z80.utils.DummyProgram

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Main extends App {
  println("START")
  val program=args.length match {
    case n if n>=1 => Program.parse(readFile(args(0)))
    case _ =>
      println("No arguments found, using dummy program.")
      Right(DummyProgram.program)
  }
  val seconds=args.length match {
    case 2 => Some(args(1).toInt)
    case _ => None
  }
  program match {
    case Left(errorMsg)=>println(errorMsg)
    case Right(prog)=>runProgram(prog,seconds)
  }
  println("END")

  private def runProgram(program: Program,seconds:Option[Int]):Unit = {
    Runner(program,seconds)
      .list
      .run
      .showCurrentLine()
      .showExitCode()
      .showSteps()
      .showTime()
  }
  private def readFile(path:String):List[String] = Files.readAllLines(Path.of(path)).asScala.toList
}

case class Runner(program: Program, maxSeconds:Option[Int]) {
  lazy val initialEnvironment:Environment=Environment.load(program)

  def list:Runner = {
    program.show()
    this
  }
  def run:Environment = {
    def endFunction:Environment=>Boolean=
      maxSeconds
        .map(secs=>Environment.finishAfterSeconds(_,secs))
        .getOrElse(Environment.finishByCode)
    Iterator[Environment](endFunction).iterate(initialEnvironment)
  }
}