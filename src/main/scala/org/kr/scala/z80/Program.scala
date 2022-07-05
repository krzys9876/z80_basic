package org.kr.scala.z80

class Program(lines:Vector[Line]) {
  def show():Unit=lines.foreach(line=>println(line.print))
}

trait Printable {
  def print:String
  def printName:String=this.getClass.getSimpleName
}

class Line(val number:Int,val statement:Statement,val tokens:List[Token]) extends Printable {
  override def print:String={
    val txtLineNum=f"$number "
    val txtStatement=f"${statement.print} "
    val txtTokens=tokens.foldLeft("")((text,token)=>text+token.print+" ")
    txtLineNum+txtStatement+txtTokens
  }
}

trait Statement extends Printable {
  override def print:String=printName
}

trait Token extends Printable

class FOR extends Statement

object FOR {
  def apply():FOR=new FOR
}

class PRINT extends Statement

object PRINT {
  def apply():PRINT=new PRINT
}

class NEXT extends Statement

object NEXT {
  def apply():NEXT=new NEXT
}

trait Keyword extends Token {
  override def print:String=printName
}

class TO extends Keyword

object TO {
  def apply():TO=new TO
}

class STEP extends Keyword

object STEP {
  def apply():STEP=new STEP
}

class Expression(source:String) extends Token {
  override def print:String=source
}

object Expression {
  def apply(source:String):Expression=new Expression(source)
}

class Assignment extends Token {
  override def print:String="="
}

object Assignment {
  def apply():Assignment=new Assignment
}

class Variable(name:String) extends Token {
  override def print:String=name
}

object Variable {
  def apply(name:String):Variable=new Variable(name)
}