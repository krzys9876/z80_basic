package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{LineNumber, Program, Statement, Variable, VariableStatic}

import scala.annotation.tailrec
import scala.math.BigDecimal

case class Environment(
                   private val variables:Variables,
                   private val data:Data,
                   private val forStack:ForStack,
                   private val lineStack:LineStack,
                   console:EnvConsole,
                   exitCode:ExitCode=ExitCode.NORMAL,
                   nextLineNum:Option[LineNumber]=None) {
  def setValue(variable: Variable, value:Any):Environment=
    processVariables(variables.store(variable,value,this))
  private def processVariables(setValueResult:Either[ExitCode,Variables]):Environment =
    setValueResult match {
      case Right(vars)=>copy(variables=vars)
      case Left(code)=>setExitCode(code)
    }
  def getValue(variable: Variable):Either[ExitCode,Any]=
    variables.value(variable,this)
  def getValueAs[T](variable: Variable):Either[ExitCode,T]=
    variables.value(variable,this).map(_.asInstanceOf[T])
  def setArrayDim(variableStatic: VariableStatic):Environment =
    copy(variables=variables.setArrayDim(variableStatic))

  def readData:Either[ExitCode,(Environment,Any)] =
    data.read match {
      case (Left(code),_) => Left(code)
      case (Right(value),newData)=>Right(copy(data=newData),value)
    }
  def storeData(values:List[Any]):Environment = copy(data=data.store(values))

  def setLine(num:LineNumber):Environment= copy(lineStack = lineStack.changeTopTo(num))
  def forceNextLine(num:LineNumber):Environment= copy(nextLineNum = Some(num))
  def setForStack(variable:Variable, line:LineNumber,
                  start:BigDecimal, end:BigDecimal, step:BigDecimal,
                  forStatus: ForStatus=ForStatus.STARTED):Environment=
    copy(forStack=forStack.push(variable,ForState(variable,start,end,step,line,forStatus)))
  def clearForStack(variable:Variable):Environment= copy(forStack=forStack.pop(variable))
  def finishForStack(variable:Variable):Environment= {
    val forState=getFor(variable).map(state=>
      ForState(variable,state.start,state.end,state.step,state.forLine,ForStatus.FINISHED))
    forState.map(state=>copy(forStack=forStack.push(variable,state))).getOrElse(this)
  }
  def pushLine(nextLine:LineNumber):Environment= {
    getCurrentLine match {
      case None => setExitCode(ExitCode.FATAL_LINE_NOT_FOUND)
      case Some(line)=>copy(lineStack=lineStack.push(line)).forceNextLine(nextLine)
    }
  }
  def popLine(program: Program):Environment= copy(lineStack=lineStack.pop).setNextLineAfterReturn(program)
  private def setNextLineAfterReturn(program: Program):Environment =
    getCurrentLine match {
      case None => setExitCode(ExitCode.MISSING_RETURN_LINE)
      case Some(line) =>
        program.lineAfter(line) match {
          case Left(code)=>setExitCode(code)
          case Right(line)=>forceNextLine(line.number)
        }
    }

  def getFor(variable:Variable):Option[ForState]=forStack.lineFor(variable)
  def getFor(variable:Option[Variable]):Option[ForState]=
    variable match {
      case None=>getCurrentLine.flatMap(forStack.lineFor)
      case Some(forVar)=>getFor(forVar)
    }

  def initFor(variable:Variable, value:BigDecimal, start:BigDecimal, end:BigDecimal, step:BigDecimal):Environment =
    getCurrentLine match {
      case None=>setExitCode(ExitCode.FATAL_LINE_NOT_FOUND)
      case Some(lineNum)=>
        setValue(variable, value)
          .setForStack(variable,lineNum,start,end,step)
    }

  def continueFor(variable: Variable, nextValue:BigDecimal):Environment = setValue(variable, nextValue)

  def finishFor(program: Program, variable: Variable):Environment =
    getCurrentLine match {
      case None => setExitCode(ExitCode.FATAL_LINE_NOT_FOUND)
      case Some(lineNum) =>
        program.getNextFor(variable, lineNum) match {
          case Some(nextLine) => this
            .finishForStack(variable)
            .forceNextLine(nextLine) // end of loop
          case None => setExitCode(ExitCode.MISSING_NEXT)
        }
    }

  def getCurrentLine:Option[LineNumber]=lineStack.top
  def initBeforeProgram:Environment=setExitCode(ExitCode.NORMAL)
  def setExitCode(code:ExitCode):Environment = copy(exitCode=code)

  def run(program:Program):Environment= {
    initBeforeProgram
      .runProgram(program,Statement.preprocess)
      .initBeforeProgram
      .runProgram(program,Statement.execute)
  }

  private def runProgram(program: Program,
                         runFunction:Statement.processLineType) = runLine(program.firstLineNumber,program,runFunction)
  @tailrec
  private final def runLine(action:Either[ExitCode,LineNumber], program: Program,
                            runFunction:Statement.processLineType):Environment= {
    action match {
      case Left(code) =>setExitCode(code) // end of program
      case Right(lineNum) =>
        val (afterEnv,nextAction)=
          resetNextLine
            .runOneLine(lineNum,program,runFunction)
        afterEnv.runLine(nextAction,program,runFunction)
    }
  }

  private def resetNextLine:Environment=copy(nextLineNum=None)
  private def runOneLine(lineNum:LineNumber,program: Program,
                         runFunction:Statement.processLineType):(Environment,Either[ExitCode,LineNumber]) = {
    program.lineByNum(lineNum) match {
      case Left(code)=>(this,Left(code))
      case Right(lineToExecute)=>
        lineToExecute.execute(program,this.setLine(lineNum),runFunction) match {
          case env if env.exitCode!=ExitCode.NORMAL => (env,Left(env.exitCode))
          case env =>
            // determine next line - either next line in program (or end of program code) or other number saved by the executed line
            val nextAction=env.nextLineNum.map(Right(_)).getOrElse(program.lineNumAfter(lineToExecute))
            (env,nextAction)
        }
    }
  }

  def consolePrint(text:String):Environment=copy(console=console.add(text,show = true))
  def consolePrintln(text:String):Environment=copy(console=console.add(text+"\n",show = true))

  def showConsole():Environment = {
    console.show()
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
  def empty:Environment=new Environment(Variables.empty,Data.empty,ForStack.empty,LineStack.empty,EnvConsole.empty)
}

case class EnvConsole(lines:Vector[String]) {
  def add(text:String,show:Boolean):EnvConsole = {
    if(show) print(text)
    copy(lines = lines :+ text)
  }
  def show():Unit = println(lines.mkString(""))
  lazy val isEmpty:Boolean = lines.isEmpty
  def contains(list:List[String]):Boolean =
    lines.length==list.length && lines.zip(list).forall({case(console,check)=>console==check})
}

object EnvConsole {
  def empty:EnvConsole=new EnvConsole(Vector())
}