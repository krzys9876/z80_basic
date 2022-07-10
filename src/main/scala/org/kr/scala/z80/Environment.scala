package org.kr.scala.z80

import scala.annotation.tailrec

class Environment(
                   private val variables:Map[Variable,Any],
                   private val forStack:ForStack,
                   private val lineStack:LineStack,
                   val console:List[String],
                   val nextLineNum:Option[LineNumber]=None) {
  def setVariable(variable: Variable,value:Any):Environment=
    new Environment(variables ++ Map(variable->value),forStack,lineStack,console)
  def getValue(variable: Variable):Option[Any]=variables.get(variable)
  def setLine(num:LineNumber):Environment={
    val newLineStack=lineStack.changeTopTo(num)
    new Environment(variables,forStack,newLineStack,console)
  }
  def setNextLine(num:LineNumber):Environment={
    new Environment(variables,forStack,lineStack,console,Some(num))
  }
  def setForStack(variable:Variable, line:LineNumber, forStatus: ForStatus=ForStatus.STARTED):Environment=
    new Environment(variables,forStack.push(variable,ForState(variable,line,forStatus)),lineStack,console)
  def clearForStack(variable:Variable):Environment=
    new Environment(variables,forStack.pop(variable),lineStack,console)
  def finishForStack(variable:Variable):Environment= {
    val forState=getFor(variable).map(state=>ForState(variable,state.forLine,ForStatus.FINISHED))
    forState.map(state=>new Environment(variables,forStack.push(variable,state),lineStack,console)).getOrElse(this)
  }

  def getFor(variable:Variable):Option[ForState]=forStack.lineFor(variable)
  def getFor(beforeLine:LineNumber):Option[ForState]=forStack.lineFor(beforeLine)
  def getCurrentLine:Option[LineNumber]=lineStack.top

  def run(program:Program):Environment= {
    runLine(program.firstLineNumber,program)
  }

  @tailrec
  final def runLine(lineNum:Option[LineNumber], program: Program):Environment= {
    lineNum match {
      case None | Some(LineNumber(_,true)) =>this // end of program
      case Some(lineNm) =>
        // init environment with current line
        val initialEnv = setLine(lineNm)
        // find line by number (it should be there)
        val line=program.line(lineNm)
        line match {
          case None=>this // TODO: Throw error???
          case Some(lineToExecute)=>
            val afterEnv=lineToExecute.execute(program,initialEnv)
            // determine next line - either next line in program of other number saved by the executed line
            val nextLineNum=afterEnv.nextLineNum.orElse(program.lineNumAfter(lineToExecute))
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

sealed trait ForStatus

object ForStatus {
  case object STARTED extends ForStatus
  case object FINISHED extends ForStatus
}

case class ForState(variable:Variable, forLine:LineNumber, status:ForStatus)

object ForState {
  def apply(variable:Variable, forLine:LineNumber):ForState=new ForState(variable,forLine,ForStatus.STARTED)
}

class ForStack(private val map:Map[Variable,ForState]) {
  def isEmpty:Boolean=map.isEmpty
  def push(variable:Variable,state:ForState):ForStack=new ForStack(map ++ Map(variable->state))
  def pop(variable:Variable):ForStack=new ForStack(map.removed(variable))

  // find 'for' statement for a given variable of any 'for' before given line number
  def lineFor(variable:Variable):Option[ForState]= map.get(variable)
  def lineFor(beforeLineNum:LineNumber):Option[ForState]= findLineBefore(beforeLineNum)

  private def findLineBefore(beforeNum: LineNumber):Option[ForState]={
    map.values.toList.filter(_.forLine.num<beforeNum.num)
      .foldLeft(Option.empty[ForState])(
        (returnLine, state) =>
          returnLine match {
            // first pass
            case None => Some(state)
            // another pass
            case Some(accumulatedState) =>
              if (state.forLine.num > accumulatedState.forLine.num) Some(state) else Some(accumulatedState)
          }
        )
  }
}

object ForStack {
  def empty:ForStack=new ForStack(Map())
}

class LineStack(private val stack:List[LineNumber]) {
  def top:Option[LineNumber]=
    stack match {
      case Nil=>None
      case head::_=>Some(head)
    }
  def push(num:LineNumber):LineStack=new LineStack(List(num) ++ stack)
  def pop:LineStack=
    stack match {
      case Nil=>this // or throw exception - TBD
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
