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
  override def execute(args:List[Token],environment: Environment):Environment=environment
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

case class Expression(source:Any) extends Token {
  override def list:String=
    source match {
      case s: String=>s
      case n:BigDecimal=>n.toString()
      case n:Int=>n.toString
      case n:Long=>n.toString
      case n:Double=>n.toString
      case b:Boolean=>b.toString
    }

  def result:Any=source
}

object Expression {
  def apply(source:String):Expression=new Expression(source)
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
