package org.kr.scala.z80

class Environment(private val variables:Map[String,Double],private val forStack:ForStack,private val lineStack:LineStack)

class ForStack(private val map:Map[String,Int]) {
  def isEmpty:Boolean=map.isEmpty
  def push(name:String,line:Int):ForStack=new ForStack(map ++ Map(name->line))
  def pop(name:String):ForStack=new ForStack(map.removed(name))
  def lineFor(name:String):Option[Int]=map.get(name)
}

object ForStack {
  def empty:ForStack=new ForStack(Map())
}

class LineStack(private val stack:List[Int]) {
  def top:Option[Int]=
    stack match {
      case Nil=>None
      case head::_=>Some(head)
    }
  def push(num:Int):LineStack=new LineStack(List(num) ++ stack)
  def pop:LineStack=
    stack match {
      case Nil=>this // or throw exception - TBD
      case head::tail=>new LineStack(tail)
    }
  def changeTopTo(newTop:Int):LineStack=
    stack match {
      case Nil=>this // or throw exception - TBD
      case head::Nil=>new LineStack(List(newTop))
      case head::tail=>new LineStack(List(newTop) ++ tail)
    }
}

object LineStack {
  def empty:LineStack=new LineStack(List())
}
