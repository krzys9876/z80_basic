package org.kr.scala.z80

class LineStack(private val stack:List[LineNumber]) {
  def top:Option[LineNumber]=
    stack match {
      case Nil=>None
      case head::_=>Some(head)
    }
  def push(num:LineNumber):LineStack=new LineStack(List(num) ++ stack)
  def pop:LineStack=
    stack match {
      case Nil=>this // TODO: or throw exception - TBD
      case head::tail=>new LineStack(tail)
    }
  def changeTopTo(newTop:LineNumber):LineStack=
    stack match {
      case Nil=>new LineStack(List(newTop)) // initialize stack
      case head::Nil=>new LineStack(List(newTop))
      case head::tail=>new LineStack(List(newTop) ++ tail)
    }
}

object LineStack {
  def empty:LineStack=new LineStack(List())
}
