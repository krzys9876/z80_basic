package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{LineNumber, Program, Variable}

import java.util.AbstractMap.SimpleEntry
import scala.annotation.tailrec
import scala.math.BigDecimal

case class Environment(
                   private val variables:Variables,
                   private val forStack:ForStack,
                   private val lineStack:LineStack,
                   console:Vector[String],
                   exitCode:ExitCode=ExitCode.NORMAL,
                   nextLineNum:Option[LineNumber]=None) {
  def setValue(variable: Variable, value:Any):Environment=
    processVariables(variables.store(variable,value))
  def setValue(variableCoords: VariableCoordinates, value:Any):Environment=
    processVariables(variables.store(variableCoords,value))
  private def processVariables(setValueResult:Either[ExitCode,Variables]):Environment =
    setValueResult match {
      case Right(vars)=>copy(variables=vars)
      case Left(code)=>setExitCode(code)
    }
  def getValue(variable: Variable):Option[Any]=variables.value(variable)
  def getValue(variableCoordinates: VariableCoordinates):Option[Any]=variables.value(variableCoordinates)
  def getValueAs[T](variable: Variable):Option[T]=variables.value(variable).map(_.asInstanceOf[T])
  def setLine(num:LineNumber):Environment= copy(lineStack = lineStack.changeTopTo(num))
  def forceNextLine(num:LineNumber):Environment= copy(nextLineNum = Some(num))
  def setForStack(variable:Variable, line:LineNumber,
                  start:BigDecimal,end:BigDecimal,step:BigDecimal,
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

  def initFor(variable:Variable,value:BigDecimal,start:BigDecimal,end:BigDecimal,step:BigDecimal):Environment =
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

  private def resetNextLine:Environment=copy(nextLineNum=None)
  private def runOneLine(lineNum:LineNumber,program: Program):(Environment,Either[ExitCode,LineNumber]) = {
    program.lineByNum(lineNum) match {
      case Left(code)=>(this,Left(code))
      case Right(lineToExecute)=>
        lineToExecute.execute(program,this.setLine(lineNum)) match {
          case env if env.exitCode!=ExitCode.NORMAL => (env,Left(env.exitCode))
          case env =>
            // determine next line - either next line in program (or end of program code) or other number saved by the executed line
            val nextAction=env.nextLineNum.map(Right(_)).getOrElse(program.lineNumAfter(lineToExecute))
            (env,nextAction)
        }
    }
  }

  //TODO: create separate class with console and enable dynamic printing to screen
  def consolePrint(text:String):Environment=copy(console=console++Vector(text))
  def consolePrintln(text:String):Environment=copy(console=console++Vector(text+"\n"))

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
  def empty:Environment=new Environment(Variables.empty,ForStack.empty,LineStack.empty,Vector())
}

case class Variables(values:Map[VariableCoordinates,Any],dimensions:Map[Variable,Dimensions]) {
  def value(variable: Variable):Option[Any]=values.get(VariableCoordinates(variable))
  def value(variableCoordinates: VariableCoordinates):Option[Any]=values.get(variableCoordinates)

  def store(variable: Variable, valueToStore:Any):Either[ExitCode,Variables]= {
    checkDimensions(variable) match {
      case Some(d) if d==Dimensions.empty =>Right(storeValue(VariableCoordinates(variable),valueToStore))
      case None =>Right(storeValue(VariableCoordinates(variable),valueToStore))
      case _ =>Left(ExitCode.INVALID_DIMENSIONALITY)
    }
  }
  def store(variableCoords: VariableCoordinates, valueToStore:Any):Either[ExitCode,Variables]= {
    checkDimensions(variableCoords.variable) match {
      case None =>
        Right(storeValue(variableCoords,valueToStore)
          .storeDimensions(variableCoords))
      case Some(d) if variableCoords.coords.check(d) => Right(storeValue(variableCoords,valueToStore))
      case _=>Left(ExitCode.INVALID_DIMENSIONALITY)
    }
  }
  private def storeValue(variable: VariableCoordinates,valueToStore:Any):Variables=
    copy(values=values ++ Map(variable->valueToStore))
  private def storeDimensions(variableCoords: VariableCoordinates):Variables=
    copy(dimensions=dimensions ++ Map(variableCoords.variable-> Dimensions.blank(variableCoords.coords)))
  private def checkDimensions(variable: Variable):Option[Dimensions] = dimensions.get(variable)
}

object Variables {
  def empty:Variables=new Variables(Map(),Map())
}

case class VariableCoordinates(variable:Variable,coords:Dimensions)

object VariableCoordinates {
  def apply(variable: Variable):VariableCoordinates = new VariableCoordinates(variable,Dimensions.empty)
}

// Contains a list of numbers representing size of ech dimension.
// E.g.: (5,10) means first dimension is 5-elements long, second dimension is 10 elements long.
// According to MS Basic documentation the default dimension size is 10, if not specified by DIM statement
case class Dimensions(dimensions: List[Int]) {
  //TODO: implement dimension check
  def check(dimensions: Dimensions):Boolean = true
  def length:Int=dimensions.length
}

object Dimensions {
  val DEFAULT_DIM:Int=10
  def empty:Dimensions = new Dimensions(List())
  def blank(dimensions: Dimensions):Dimensions = {
    val dimLen=dimensions.length
    new Dimensions(List.fill(dimLen)(DEFAULT_DIM))
  }
}