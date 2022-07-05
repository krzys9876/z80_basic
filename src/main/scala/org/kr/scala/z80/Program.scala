package org.kr.scala.z80

class Program(val lines:Vector[Line]) {
  def show():Unit=lines.foreach(line=>println(line.list))
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
  def execute(env:Environment):Environment=env
}

trait Statement extends Listable {
  override def list:String=listName
}

trait Token extends Listable

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

class Expression(source:String) extends Token {
  override def list:String=source
}

object Expression {
  def apply(source:String):Expression=new Expression(source)
}

class Assignment extends Token {
  override def list:String="="
}

object Assignment {
  def apply():Assignment=new Assignment
}

class Variable(name:String) extends Token {
  override def list:String=name
}

object Variable {
  def apply(name:String):Variable=new Variable(name)
}