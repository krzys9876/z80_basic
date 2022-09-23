package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{Program, Statement, StatementId, Variable, VariableStatic}

import scala.annotation.tailrec
import scala.math.BigDecimal

case class Environment(
                        private val variables:Variables,
                        private val data:Data,
                        private val forStack:ForStack,
                        private val lineStack:LineStack,
                        console:EnvConsole,
                        exitCode:ExitCode=ExitCode.NORMAL,
                        nextStatement:Option[StatementId]=None) {
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
  def setArrayDim(variable: Variable):Environment = {
    variable.evaluateIndex(this) match {
      case Left(code)=>setExitCode(code)
      case Right(index)=>copy(variables=variables.setArrayDim(VariableStatic(variable.name,index)))
    }
  }

  def readData:Either[ExitCode,(Environment,Any)] =
    data.read match {
      case (Left(code),_) => Left(code)
      case (Right(value),newData)=>Right(copy(data=newData),value)
    }
  def storeData(values:List[Any]):Environment = copy(data=data.store(values))

  def setLine(num:StatementId):Environment= copy(lineStack = lineStack.changeTopTo(num))
  def forceNextLine(num:StatementId):Environment= copy(nextStatement = Some(num))
  def setForStack(variable:Variable, line:StatementId,
                  start:BigDecimal, end:BigDecimal, step:BigDecimal,
                  forStatus: ForStatus=ForStatus.STARTED):Environment=
    copy(forStack=forStack.push(variable,ForState(variable,start,end,step,line,forStatus)))
  def clearForStack(variable:Variable):Environment= copy(forStack=forStack.pop(variable))
  def finishForStack(variable:Variable):Environment= {
    val forState=getFor(variable).map(state=>
      ForState(variable,state.start,state.end,state.step,state.forLine,ForStatus.FINISHED))
    forState.map(state=>copy(forStack=forStack.push(variable,state))).getOrElse(this)
  }
  def pushLine(nextLine:StatementId):Environment= {
    getCurrentStatement match {
      case None => setExitCode(ExitCode.FATAL_LINE_NOT_FOUND)
      case Some(line)=>copy(lineStack=lineStack.push(line)).forceNextLine(nextLine)
    }
  }
  def popLine(program: Program):Environment = {
    lineStack.pop match {
      case Left(code)=>setExitCode(code)
      case Right(newLineStack)=>copy(lineStack=newLineStack).setNextLineAfterReturn(program)
    }
  }
  private def setNextLineAfterReturn(program: Program):Environment =
    getCurrentStatement match {
      case None => setExitCode(ExitCode.MISSING_RETURN_LINE)
      case Some(statement) =>
        program.statementAfter(statement) match {
          case Left(code)=>setExitCode(code)
          case Right(line)=>forceNextLine(line)
        }
    }

  def getFor(variable:Variable):Option[ForState]=forStack.lineFor(variable)
  def getFor(variable:Option[Variable]):Option[ForState]=
    variable match {
      case None=>getCurrentStatement.flatMap(forStack.lineFor)
      case Some(forVar)=>getFor(forVar)
    }

  def initFor(variable:Variable, value:BigDecimal, start:BigDecimal, end:BigDecimal, step:BigDecimal):Environment =
    getCurrentStatement match {
      case None=>setExitCode(ExitCode.FATAL_LINE_NOT_FOUND)
      case Some(statement)=>
        setValue(variable, value)
          .setForStack(variable,statement,start,end,step)
    }

  def continueFor(variable: Variable, nextValue:BigDecimal):Environment = setValue(variable, nextValue)

  def finishFor(program: Program, variable: Variable):Environment =
    getCurrentStatement match {
      case None => setExitCode(ExitCode.FATAL_LINE_NOT_FOUND)
      case Some(lineNum) =>
        program.getNextFor(variable, lineNum) match {
          case Some(nextStatement) => this
            .finishForStack(variable)
            .forceNextLine(nextStatement) // end of loop
          case None => setExitCode(ExitCode.MISSING_NEXT)
        }
    }

  def getCurrentStatement:Option[StatementId]=lineStack.top
  def initBeforeProgram:Environment=setExitCode(ExitCode.NORMAL)
  def setExitCode(code:ExitCode):Environment = copy(exitCode=code)

  def run(program:Program):Environment= {
    initBeforeProgram
      .runProgram(program,Statement.preprocess)
      .initBeforeProgram
      .runProgram(program,Statement.execute)
  }

  private def runProgram(program: Program,
                         runFunction:Statement.processLineType) = runLine(program.firstStatement,program,runFunction)
  @tailrec
  private final def runLine(action:Either[ExitCode,StatementId], program: Program,
                            runFunction:Statement.processLineType):Environment= {
    action match {
      case Left(code) =>setExitCode(code) // end of program
      case Right(statementId) =>
        val (afterEnv,nextAction)=
          resetNextLine
            .runOneLine(statementId,program,runFunction)
        afterEnv.runLine(nextAction,program,runFunction)
    }
  }

  private def resetNextLine:Environment=copy(nextStatement=None)
  private def runOneLine(statementId:StatementId, program: Program,
                         runFunction:Statement.processLineType):(Environment,Either[ExitCode,StatementId]) = {
    program.lineByNum(statementId) match {
      case Left(code)=>(this,Left(code))
      case Right(lineToExecute)=>
        lineToExecute.execute(program,setLine(statementId),statementId,runFunction) match {
          case env if env.exitCode!=ExitCode.NORMAL => (env,Left(env.exitCode))
          case env =>
            // determine next line - either next line in program (or end of program code) or other number saved by the executed line
            val nextAction=env.nextStatement.map(num=>Right(num)).getOrElse(program.statementAfter(statementId))
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