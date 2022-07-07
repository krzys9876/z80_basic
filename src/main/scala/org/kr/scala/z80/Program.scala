package org.kr.scala.z80

class Program(val lines:Vector[Line]) {
  def show():Unit=lines.foreach(line=>println(line.list))
  def firstLineNumber:Option[LineNumber]=
    if(lines.isEmpty) None
    else Some(lines(0).number)
  def lineAfter(line:Line):Option[Line]={
    val index=lines.indexOf(line)
    index match {
      case i if i<0=> None // TODO: Throw exception???
      case i if i==lines.length-1 => None // end of program
      case i=>Some(lines(i+1))
    }
  }
  def lineNumAfter(line:Line):Option[LineNumber]=
    lineAfter(line).map(_.number)
      .orElse(Some(LineNumber(Int.MaxValue,endOfProgram = true)))
  def line(lineNum:LineNumber):Option[Line]=lines.find(_.number==lineNum)

  def getNextFor(variable: Variable,from:LineNumber):Option[LineNumber]={
    val forLine=lines.find(_.number==from)
    val forLineIndex=forLine.map(lines.indexOf).getOrElse(-1)
    if(forLineIndex>=0) {
      lines
        .slice(forLineIndex,lines.length)
        .find(_.isNextFor(variable))
        .flatMap(lineNumAfter)
    }
    else None
  }
}

trait Listable {
  def list:String
  def listName:String=this.getClass.getSimpleName
}

case class LineNumber(num:Int,endOfProgram:Boolean=false) {
  override def toString: String = num.toString
}

class Line(val number:LineNumber,val statement:Statement,val tokens:List[Token]) extends Listable {
  override def list:String={
    val txtLineNum=f"$number "
    val txtStatement=f"${statement.list} "
    val txtTokens=tokens.foldLeft("")((text,token)=>text+token.list+" ")
    txtLineNum+txtStatement+txtTokens
  }
  def execute(program:Program,env:Environment):Environment={
    val newEnv=env.setLine(number)
    statement.execute(tokens,program,newEnv)
  }

  def isNextFor(variable: Variable):Boolean={
    statement.isInstanceOf[NEXT] &&
      statement.asInstanceOf[NEXT].variable==variable
  }
}

trait Statement extends Listable {
  override def list:String=listName
  def execute(args:List[Token],program:Program,environment:Environment):Environment
}

trait Token extends Listable

class FOR extends Statement {
  override def execute(args:List[Token],program:Program,environment: Environment):Environment= {
    val(argAssign,argTo,argEndVal,argsAfterEndVal)=decodeArgs(args)
    val argNextStmt= program
      .getNextFor(argAssign.get.variable,environment.getCurrentLine.get)

    if(argAssign.isEmpty || argTo.isEmpty || argEndVal.isEmpty) environment // TODO: Throw error???
    else {
      val lineFor = environment.getFor(argAssign.get.variable.name)
      if (lineFor.isEmpty) { // start of loop
        environment
          .setVariable(argAssign.get.variable, argAssign.get.expression)
          .setForStack(argAssign.get.variable.name,environment.getCurrentLine.get)
      } else {
        val nextValueResult=environment.getValue(argAssign.get.variable).get.asInstanceOf[Result]
        val nextValue=nextValueResult.resultNum.get+1

        if(nextValue>argEndVal.get.resultNum.get)
          environment.setNextLine(argNextStmt.get) // end of loop
        else
          environment
          .setVariable(argAssign.get.variable, Result(nextValue)) // TODO: check for empty

      }
    }
  }

  private def decodeArgs(args:List[Token]):(Option[Assignment],Option[TO],Option[Expression],List[Token])={
    args.take(3) match {
      case assignment :: to :: endValue :: _
        if assignment.isInstanceOf[Assignment] && to.isInstanceOf[TO] &&
          endValue.isInstanceOf[Expression] =>
        (Some(assignment.asInstanceOf[Assignment]),
          Some(to.asInstanceOf[TO]),
          Some(endValue.asInstanceOf[Expression]),
          args.slice(3,args.length))
      case _ => (None,None,None,args)
    }
  }
}

object FOR {
  def apply():FOR=new FOR
}

class NEXT(val variable: Variable) extends Statement {
  override def execute(args:List[Token],program:Program,environment: Environment):Environment= {
    environment
      .getFor(variable.name)
      .map(environment.setNextLine)
      .getOrElse(environment)
  }
}

object NEXT {
  def apply(variable: Variable):NEXT=new NEXT(variable)
}

class PRINT(val expression: Expression) extends Statement {
  // print text to console
  override def execute(args:List[Token],program:Program,environment: Environment):Environment= {
    val output=expression.result.toString
    environment.consolePrintln(output)
  }
}

object PRINT {
  def apply(expression: Expression):PRINT=new PRINT(expression)
}

trait Keyword extends Token {
  override def list:String=listName
}

class TO extends Keyword

object TO {
  def apply():TO=new TO
}

class STEP extends Keyword

object STEP {
  def apply():STEP=new STEP
}

abstract class Expression extends Token {
  override def list:String=
    result match {
      case s: String=>s
      case n:BigDecimal=>n.toString()
      case n:Int=>n.toString
      case n:Long=>n.toString
      case n:Double=>n.toString
      case b:Boolean=>b.toString
      case _=>"TYPE NOT SUPPORTED"
    }

  val result:Any
  val resultNum:Option[BigDecimal]
  val resultText:Option[String]
}

case class Result(value:Any) extends Expression {
  override val result:Any=value
  override val resultNum:Option[BigDecimal]=
    value match {
      case n:BigDecimal=>Some(n)
      case _=>None
    }
  override val resultText:Option[String]=
    value match {
      case s:String=>Some(s)
      case _=>None
    }
}

object Result {
  def apply(value:Any):Result= {
    val valueTyped=value match {
      case n:Int=>BigDecimal(n)
      case n:Long=>BigDecimal(n)
      case n:Double=>BigDecimal(n)
      case n:BigDecimal=>n
      case b:Boolean=>BigDecimal(if(b) 1 else 0)
      case s:String=>s
    }
    new Result(valueTyped)
  }
}

class Assignment(val variable:Variable,val expression:Expression) extends Token {
  override def list:String=f"${variable.list}=${expression.list}"
}

object Assignment {
  def apply(variable:Variable,expression: Expression):Assignment=new Assignment(variable,expression)
}

case class Variable(name:String) extends Token {
  override def list:String=name
}

object Variable {
  def apply(name:String):Variable=new Variable(name)
}

class REM extends Statement {
  // ignore the line
  override def execute(args:List[Token],program:Program,environment: Environment):Environment=environment
}

object REM {
  def apply():REM=new REM
}

class LET extends Statement {
  // print text to console
  override def execute(args:List[Token],program:Program,environment: Environment):Environment= {
    val assign=assignment(args)
    if(assign.isEmpty) environment // TODO: Throw error???
    else environment.setVariable(assign.get.variable,assign.get.expression)
  }

  private def assignment(args:List[Token]):Option[Assignment]={
    args match {
      case head::_ =>
        head match {
          case assign:Assignment=>Some(assign)
          case _=> None
        }
      case Nil =>None
    }
  }
}

object LET {
  def apply():LET=new LET
}

