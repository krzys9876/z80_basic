package org.kr.scala.z80

class Environment(
                   private val variables:Map[Variable,Any],
                   private val forStack:ForStack,
                   private val lineStack:LineStack,
                   val console:List[String]) {
  def setVariable(variable: Variable,value:Any):Environment=
    new Environment(variables ++ Map(variable->value),forStack,lineStack,console)
  def getValue(variable: Variable):Option[Any]=variables.get(variable)
  def setLine(num:Int):Environment={
    val newLineStack=lineStack.changeTopTo(num)
    new Environment(variables,forStack,newLineStack,console)
  }
  def setForStack(name:String,line:Int):Environment={
    val newForStack=forStack.push(name,line)
    new Environment(variables,newForStack,lineStack,console)
  }
  def getFor(name:String):Option[Int]=forStack.lineFor(name)
  def getCurrentLine:Option[Int]=lineStack.top

  def run(program:Program):Environment= {
    program.firstLineNumber.map(doRun(_,program)).getOrElse(this)
  }

  private def doRun(startLineNum:Int,program: Program):Environment= {
    val initialEnv = new Environment(variables, forStack, lineStack.push(startLineNum), console)
    program.lines.foldLeft(initialEnv)((env, line) => line.execute(env))
  }

  def consolePrint(text:String):Environment=new Environment(variables,forStack,lineStack,console++List(text))
  def consolePrintln(text:String):Environment=new Environment(variables,forStack,lineStack,console++List(text+"\n"))

  def showConsole():Unit = println(console.mkString(""))
}

object Environment {
  def empty:Environment=new Environment(Map(),ForStack.empty,LineStack.empty,List())
}

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
