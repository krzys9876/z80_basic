package org.kr.scala.z80

class Program(val lines:Vector[Line]) {
  def show():Unit=lines.foreach(line=>println(line.list))
  def firstLineNumber:Option[Int]=
    if(lines.isEmpty) None
    else Some(lines(0).number)
}

trait Listable {
  def list:String
  def listName:String=this.getClass.getSimpleName
}

class Line(val number:Int,val statement:Statement,val tokens:List[Token]) extends Listable {
  override def list:String={
    val txtLineNum=f"$number "
    val txtStatement=f"${statement.list} "
    val txtTokens=tokens.foldLeft("")((text,token)=>text+token.list+" ")
    txtLineNum+txtStatement+txtTokens
  }
  def execute(env:Environment):Environment={
    val newEnv=env.setLine(number)
    statement.execute(tokens,newEnv)
  }
}

trait Statement extends Listable {
  override def list:String=listName
  def execute(args:List[Token],environment: Environment):Environment
}

trait Token extends Listable

class FOR extends Statement {
  override def execute(args:List[Token],environment: Environment):Environment= {
    val (argAssign,argsAfterAssignment)=assignment(args)
    val (argTo,argsAfterTo)=to(argsAfterAssignment)
    val (argEndVal,argsAfterEndVal)=endValue(argsAfterTo)


    if(argAssign.isEmpty || argTo.isEmpty || argEndVal.isEmpty) environment // TODO: Throw error???
    else {
      val lineFor = environment.getFor(argAssign.get.variable.name)
      if (lineFor.isEmpty) {
        environment
          .setVariable(argAssign.get.variable, argAssign.get.expression)
          .setForStack(argAssign.get.variable.name,10) // TODO: hardcode
      } else {
        environment
        //  .setVariable(argAssign.get.variable, Result(argAssign.get.))
      }
    }
}


  private def assignment(args:List[Token]):(Option[Assignment],List[Token])=
    args match {
      case head::_ =>
        head match {
          case assign:Assignment=>(Some(assign),args.tail)
          case _=> (None,args.tail)
        }
      case Nil =>(None,args) // TODO: throw error???
    }

  private def to(args:List[Token]):(Option[TO],List[Token])=
    args match {
      case head::_ =>
        head match {
          case to:TO=>(Some(to),args.tail)
          case _=> (None,args.tail)
        }
      case Nil =>(None,args) // TODO: throw error???
    }

  private def endValue(args:List[Token]):(Option[Expression],List[Token])=
    args match {
      case head::_ =>
        head match {
          case endV:Expression=>(Some(endV),args.tail)
          case _=> (None,args.tail)
        }
      case Nil =>(None,args) // TODO: throw error???
    }
}

object FOR {
  def apply():FOR=new FOR
}

class PRINT extends Statement {
  // print text to console
  override def execute(args:List[Token],environment: Environment):Environment= {
    val output=expression(args).map(_.result.toString).getOrElse("")
    environment.consolePrintln(output)
  }

  private def expression(args:List[Token]):Option[Expression]={
    args match {
      case head::_ =>
        head match {
          case expr:Expression=>Some(expr)
          case _=> None
        }
      case Nil =>None
    }
  }
}

object PRINT {
  def apply():PRINT=new PRINT
}

class NEXT extends Statement {
  override def execute(args:List[Token],environment: Environment):Environment=environment
}

object NEXT {
  def apply():NEXT=new NEXT
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

  def result:Any
}

case class Result(value:Any) extends Expression {
  def result:Any=value
}

object Result {
  def apply(value:Any):Result=new Result(value)
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
  override def execute(args:List[Token],environment: Environment):Environment=environment
}

object REM {
  def apply():REM=new REM
}

class LET extends Statement {
  // print text to console
  override def execute(args:List[Token],environment: Environment):Environment= {
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
