package org.kr.scala.z80.program

import org.kr.scala.z80.environment.ExitCode

class Program(val srcLines: Vector[Line]) {
  val lines:Vector[Line] = srcLines.sortBy(_.number.num)
  def show(): Unit = lines.foreach(line => println(line.list))

  def firstLineNumber: Either[ExitCode,LineNumber] =
    if (lines.isEmpty) Left(ExitCode.PROGRAM_END)
    else Right(lines(0).number)

  def lineAfter(line: Line): Option[Line] = {
    val index = lines.indexOf(line)
    index match {
      case i if i < 0 => None // TODO: Throw exception???
      case i if i == lines.length - 1 => None // end of program
      case i => Some(lines(i + 1))
    }
  }

  def lineNumAfter(line: Line): Either[ExitCode,LineNumber] = {
    lineAfter(line) match {
      case Some(lineNum)=>Right(lineNum.number)
      case None=>Left(ExitCode.PROGRAM_END)
    }
  }

  def lineByNum(lineNum: LineNumber): Either[ExitCode,Line] =
    lines
      .find(_.number == lineNum).map(Right(_))
      .getOrElse(Left(ExitCode.FATAL_LINE_NOT_FOUND))

  def getNextFor(variable: Variable, from: LineNumber): Option[LineNumber] = {
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
