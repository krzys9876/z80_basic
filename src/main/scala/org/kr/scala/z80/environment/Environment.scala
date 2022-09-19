package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{LineNumber, Program, Variable}
import org.kr.scala.z80.{environment, _}

import scala.annotation.tailrec

case class Environment(
                   private val variables:Map[Variable,Any],
                   private val forStack:ForStack,
                   private val lineStack:LineStack,
                   console:List[String],
                   exitCode:ExitCode=ExitCode.NORMAL,
                   nextLineNum:Option[LineNumber]=None) {
  def resetNextLine:Environment=copy(nextLineNum=None)
  def setVariable(variable: Variable,value:Any):Environment= copy(variables=variables ++ Map(variable->value))
  def getValue(variable: Variable):Option[Any]=variables.get(variable)
  def getValueAs[T](variable: Variable):Option[T]=variables.get(variable).map(_.asInstanceOf[T])
  def setLine(num:LineNumber):Environment= copy(lineStack = lineStack.changeTopTo(num))
  def forceNextLine(num:LineNumber):Environment= copy(nextLineNum = Some(num))
  def setForStack(variable:Variable, line:LineNumber,
                  start:BigDecimal,end:BigDecimal,step:BigDecimal, forStatus: ForStatus=ForStatus.STARTED):Environment=
    copy(forStack=forStack.push(variable,environment.ForState(variable,start,end,step,line,forStatus)))
  def clearForStack(variable:Variable):Environment= copy(forStack=forStack.pop(variable))
  def finishForStack(variable:Variable):Environment= {
    val forState=getFor(variable).map(state=>environment.ForState(variable,state.start,state.end,state.step,state.forLine,ForStatus.FINISHED))
    forState.map(state=>copy(forStack=forStack.push(variable,state))).getOrElse(this)
  }

  def getFor(variable:Variable):Option[ForState]=forStack.lineFor(variable)
  def getFor(variable:Option[Variable]):Option[ForState]=
    variable match {
      case None=>getCurrentLine.flatMap(forStack.lineFor)
      case Some(forVar)=>getFor(forVar)
    }

  def getCurrentLine:Option[LineNumber]=lineStack.top

  def run(program:Program):Environment= {
    runLine(program.firstLineNumber,program)
  }

  def setExitCode(code:ExitCode):Environment = copy(exitCode=code)

  @tailrec
  final def runLine(action:Either[ExitCode,LineNumber], program: Program):Environment= {
    action match {
      case Left(code) =>setExitCode(code) // end of program
      case Right(lineNum) =>
        val (afterEnv,nextAction)=
          resetNextLine
            .runOneLine(lineNum,program)
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

  def consolePrint(text:String):Environment=copy(console=console++List(text))
  def consolePrintln(text:String):Environment=copy(console=console++List(text+"\n"))

  def showConsole():Environment = {
    println(console.mkString(""))
    this
  }
  def showExitCode():Environment = {
    println(f"Exit code: $exitCode")
    this
  }

  def showCurrentLine():Environment = {
    println(f"Current line: ${lineStack.top.getOrElse(-1)}")
    this
  }
}

object Environment {
  def empty:Environment=new Environment(Map(),ForStack.empty,LineStack.empty,List())
}
