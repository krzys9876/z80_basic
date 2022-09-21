package org.kr.scala.z80.program

import org.kr.scala.z80.environment.ExitCode

class Program(val srcLines: Vector[Line]) {
  val lines:Vector[Line] = srcLines.sortBy(_.number.num)
  def show(): Unit = lines.foreach(line => println(line.list))

  def firstLineNumber: Either[ExitCode,LineNumber] =
    if (lines.isEmpty) Left(ExitCode.PROGRAM_END)
    else Right(lines(0).number)

  private def lineAfter(line: Line): Either[ExitCode,Line] = {
    val index = lines.indexOf(line)
    index match {
      case i if i < 0 => Left(ExitCode.INVALID_LINE)
      case i if i == lines.length - 1 => Left(ExitCode.PROGRAM_END) // end of program
      case i => Right(lines(i + 1))
    }
  }

  def lineAfter(lineNum: LineNumber): Either[ExitCode,Line] = {
    lineByNum(lineNum) match {
      case Left(code)=>Left(code)
      case Right(line)=>lineAfter(line)
    }
  }

  def lineNumAfter(line: Line): Either[ExitCode,LineNumber] =
    lineAfter(line) match {
      case Right(line)=>Right(line.number)
      case Left(code)=>Left(code)
    }

  def lineByNum(lineNum: LineNumber): Either[ExitCode,Line] =
    lines
      .find(_.number == lineNum).map(Right(_))
      .getOrElse(Left(ExitCode.FATAL_LINE_NOT_FOUND))

  def getNextFor(variable: VariableIndex, from: LineNumber): Option[LineNumber] = {
    val forLineIndex = lines
      .find(_.number == from)
      .map(lines.indexOf).getOrElse(-1)
    forLineIndex match {
      case index if index >= 0 =>
        lines
          .slice(forLineIndex, lines.length)
          .find(_.isNextFor(variable))
          .map(_.number)
          //.flatMap(lineNumAfter)
      case _ => None
    }
  }
}
