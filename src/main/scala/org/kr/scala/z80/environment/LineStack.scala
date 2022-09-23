package org.kr.scala.z80.environment

import org.kr.scala.z80.program.StatementId

class LineStack(private val stack:List[StatementId]) {
  def top:Option[StatementId]=
    stack match {
      case Nil=>None
      case head::_=>Some(head)
    }
  def push(num:StatementId):LineStack=new LineStack(List(num) ++ stack)
  def pop:Either[ExitCode,LineStack]=
    stack match {
      // this should never happen as stack is never empty during program execution
      case Nil  =>Left(ExitCode.RETURN_WITHOUT_GOSUB)
      // head contains current line - cannot pop
      case head::Nil =>Left(ExitCode.RETURN_WITHOUT_GOSUB)
      // normal case - pop previously saved line
      case head::tail=>Right(new LineStack(tail))
    }
  def changeTopTo(newTop:StatementId):LineStack=
    stack match {
      case Nil=>new LineStack(List(newTop)) // initialize stack
      case _::Nil=>new LineStack(List(newTop))
      case _::tail=>new LineStack(List(newTop) ++ tail)
    }
}

object LineStack {
  def empty:LineStack=new LineStack(List())
}
