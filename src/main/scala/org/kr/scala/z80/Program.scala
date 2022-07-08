package org.kr.scala.z80

class Program(val lines: Vector[Line]) {
  def show(): Unit = lines.foreach(line => println(line.list))

  def firstLineNumber: Option[LineNumber] =
    if (lines.isEmpty) None
    else Some(lines(0).number)

  def lineAfter(line: Line): Option[Line] = {
    val index = lines.indexOf(line)
    index match {
      case i if i < 0 => None // TODO: Throw exception???
      case i if i == lines.length - 1 => None // end of program
      case i => Some(lines(i + 1))
    }
  }

  def lineNumAfter(line: Line): Option[LineNumber] =
    lineAfter(line).map(_.number)
      .orElse(Some(LineNumber(Int.MaxValue, endOfProgram = true)))

  def line(lineNum: LineNumber): Option[Line] = lines.find(_.number == lineNum)

  def getNextFor(variable: Variable, from: LineNumber): Option[LineNumber] = {
    val forLine = lines.find(_.number == from)
    val forLineIndex = forLine.map(lines.indexOf).getOrElse(-1)
    if (forLineIndex >= 0) {
      lines
        .slice(forLineIndex, lines.length)
        .find(_.isNextFor(variable))
        .flatMap(lineNumAfter)
    }
    else None
  }
}
