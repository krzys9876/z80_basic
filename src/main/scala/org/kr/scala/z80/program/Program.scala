package org.kr.scala.z80.program

import org.kr.scala.z80.environment.ExitCode

class Program(val srcLines: Vector[Line]) {
  val lines:Vector[Line] = srcLines.sortBy(_.number.num)
  def show(): Unit = lines.foreach(line => println(line.list))

  def firstStatement: Either[ExitCode,StatementId] =
    if (lines.isEmpty) Left(ExitCode.PROGRAM_END)
    else Right(StatementId(lines(0).number,0))

  private def lineAfter(line: Line): Either[ExitCode,Line] = {
    val index = lines.indexOf(line)
    index match {
      case i if i < 0 => Left(ExitCode.INVALID_LINE)
      case i if i == lines.length - 1 => Left(ExitCode.PROGRAM_END) // end of program
      case i => Right(lines(i + 1))
    }
  }

  def lineAfter(lineNum: StatementId): Either[ExitCode,Line] = {
    lineByNum(lineNum) match {
      case Left(code)=>Left(code)
      case Right(line)=>lineAfter(line)
    }
  }

  def lineNumAfter(line: Line): Either[ExitCode,StatementId] =
    lineAfter(line) match {
      case Right(line)=>Right(StatementId(line.number,0))
      case Left(code)=>Left(code)
    }

  def statementAfter(statementId: StatementId): Either[ExitCode,StatementId] =
    lineByNum(statementId) match {
      case Left(code)=>Left(code)
      case Right(line) if statementId.statementNum<line.statementCount-1 => Right(statementId.nextSameLine)
      case Right(line)=>
        lineAfter(line) match {
          case Left(code)=>Left(code)
          case Right(line)=>Right(StatementId(line.number,0))
        }
    }

  def lineByNum(lineNum: StatementId): Either[ExitCode,Line] =
    lines
      .find(_.number == lineNum.lineNumber).map(Right(_))
      .getOrElse(Left(ExitCode.FATAL_LINE_NOT_FOUND))

  def statementById(statementId: StatementId): Either[ExitCode,(Statement,Line)] = {
    lines.find(_.number == statementId.lineNumber) match {
      case Some(line) if statementId.statementNum<line.statementCount =>
        Right((line.statements(statementId.statementNum),line))
      case None => Left(ExitCode.FATAL_LINE_NOT_FOUND)
    }
  }


  def getNextFor(variable: Variable, from: StatementId): Option[LineNumber] = {
    val forLineIndex = lines
      .find(l=>StatementId(l.number) == from)
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
