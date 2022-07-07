package org.kr.scala.z80

import scala.annotation.tailrec

class Environment(
                   private val variables:Map[Variable,Any],
                   private val forStack:ForStack,
                   private val lineStack:LineStack,
                   val console:List[String],
                   val nextLineNum:Option[Int]=None) {
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
    runLine(program.firstLineNumber,program)
  }

  @tailrec
  final def runLine(lineNum:Option[Int], program: Program):Environment= {
    lineNum match {
      case None=>this // end of program
      case Some(lineNm) =>
        // init environment with current line
        val initialEnv = setLine(lineNm)
        // find line by number (it should be there)
        val line=program.line(lineNm)
        line match {
          case None=>this // TODO: Throw error???
          case Some(lineToExecute)=>
            val afterEnv=lineToExecute.execute(initialEnv)
            // determine next line - either next line in program of other number saved by the executed line
            val nextLineNum=afterEnv.nextLineNum.orElse(program.lineAfter(lineToExecute))
            afterEnv.runLine(nextLineNum,program)
        }
    }
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
      case Nil=>new LineStack(List(newTop)) // initialize stack
      case head::Nil=>new LineStack(List(newTop))
      case head::tail=>new LineStack(List(newTop) ++ tail)
    }
}

object LineStack {
  def empty:LineStack=new LineStack(List())
}
