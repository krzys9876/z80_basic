package org.kr.scala.z80

import scala.annotation.tailrec

class Environment(
                   private val variables:Map[Variable,Any],
                   private val forStack:ForStack,
                   private val lineStack:LineStack,
                   val console:List[String],
                   val exitCode:ExitCode=ExitCode.NORMAL,
                   val nextLineNum:Option[LineNumber]=None) {
  def setVariable(variable: Variable,value:Any):Environment=
    new Environment(variables ++ Map(variable->value),forStack,lineStack,console)
  def getValue(variable: Variable):Option[Any]=variables.get(variable)
  def setLine(num:LineNumber):Environment={
    val newLineStack=lineStack.changeTopTo(num)
    new Environment(variables,forStack,newLineStack,console)
  }
  def forceNextLine(num:LineNumber):Environment=
    new Environment(variables,forStack,lineStack,console,exitCode,Some(num))
  def setForStack(variable:Variable, line:LineNumber, forStatus: ForStatus=ForStatus.STARTED):Environment=
    new Environment(variables,forStack.push(variable,ForState(variable,line,forStatus)),lineStack,console)
  def clearForStack(variable:Variable):Environment=
    new Environment(variables,forStack.pop(variable),lineStack,console)
  def finishForStack(variable:Variable):Environment= {
    val forState=getFor(variable).map(state=>ForState(variable,state.forLine,ForStatus.FINISHED))
    forState.map(state=>new Environment(variables,forStack.push(variable,state),lineStack,console)).getOrElse(this)
  }

  def getFor(variable:Variable):Option[ForState]=forStack.lineFor(variable)
  def getFor(variable:Option[Variable]):Option[ForState]=
    variable
      .flatMap(getFor)
      .orElse(getCurrentLine.flatMap(forStack.lineFor))

  def getCurrentLine:Option[LineNumber]=lineStack.top

  def run(program:Program):Environment= {
    runLine(program.firstLineNumber,program)
  }

  def setExitCode(code:ExitCode):Environment = new Environment(variables,forStack,lineStack,console,code,None)

  @tailrec
  final def runLine(action:Either[ExitCode,LineNumber], program: Program):Environment= {
    action match {
      case Left(code) =>setExitCode(code) // end of program
      case Right(lineNum) =>
        val (afterEnv,nextAction)=runOneLine(lineNum,program)
        afterEnv.runLine(nextAction,program)
    }
  }

  private def runOneLine(lineNum:LineNumber,program: Program):(Environment,Either[ExitCode,LineNumber]) = {
    program.lineByNum(lineNum) match {
      case Left(code)=>(this,Left(code))
      case Right(lineToExecute)=>
        // line execution creates new version of environment
        lineToExecute.execute(program,this.setLine(lineNum)) match {
          case env if env.exitCode!=ExitCode.NORMAL => (env,Left(env.exitCode))
          case env =>
            // determine next line - either next line in program (or end of program code) or other number saved by the executed line
            val nextAction=env.nextLineNum.map(Right(_)).getOrElse(program.lineNumAfter(lineToExecute))
            (env,nextAction)
        }
    }
  }

  def consolePrint(text:String):Environment=new Environment(variables,forStack,lineStack,console++List(text))
  def consolePrintln(text:String):Environment=new Environment(variables,forStack,lineStack,console++List(text+"\n"))

  def showConsole():Environment = {
    println(console.mkString(""))
    this
  }
  def showExitCode():Environment = {
    println(f"Exit code: $exitCode")
    this
  }
}

object Environment {
  def empty:Environment=new Environment(Map(),ForStack.empty,LineStack.empty,List())
}

sealed trait ExitCode

object ExitCode {
  case object NORMAL extends ExitCode {override def toString: String = "NORMAL"}
  case object PROGRAM_END extends ExitCode {override def toString: String = "PROGRAM_END"}
  case object FATAL_LINE_NOT_FOUND extends ExitCode {override def toString: String = "FATAL_LINE_NOT_FOUND"}
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
