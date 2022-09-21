package org.kr.scala.z80.program

import scala.language.implicitConversions

case class Variable (name:String) extends Listable {
  override def list: String = name
}

case class VariableIndex(variable:Variable, index:Index) extends Listable {
  override def list: String = f"${variable.list}${index.list}"
}

object VariableIndex {
  def apply(variable: Variable):VariableIndex = new VariableIndex(variable,Index.empty)
  def apply(name:String,index:Index):VariableIndex = new VariableIndex(Variable(name),index)
  implicit def fromString(name:String):VariableIndex = VariableIndex(Variable(name))
}


// Contains a list of numbers representing size of ech dimension.
// E.g.: (5,10) means first dimension is 5-elements long, second dimension is 10 elements long.
// According to MS Basic documentation the default dimension size is 10, if not specified by DIM statement
case class Index(dimensions: List[Int]) extends Listable {
  def fitsSize(sizeIndex: Index):Boolean = {
    sameLength(sizeIndex) && indexLessThanSize(sizeIndex)
  }
  private def sameLength(sizeIndex: Index):Boolean = length==sizeIndex.length
  private def indexLessThanSize(sizeIndex: Index):Boolean =
    dimensions.zip(sizeIndex.dimensions).forall({case(index,size)=>size>index && index>=0})

  def length:Int=dimensions.length
  def isEmpty:Boolean=dimensions.isEmpty

  override def list: String = if(isEmpty) "" else toString
  override def toString: String = f"(${dimensions.mkString(",")})"
}

object Index {
  val DEFAULT_DIM_SIZE:Int=10
  def empty:Index = new Index(List())
  def blank(dimensions: Index):Index = {
    val dimLen=dimensions.length
    new Index(List.fill(dimLen)(DEFAULT_DIM_SIZE))
  }
}
