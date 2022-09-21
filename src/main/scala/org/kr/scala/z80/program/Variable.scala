package org.kr.scala.z80.program

import org.kr.scala.z80.environment.{Environment, ExitCode}
import org.kr.scala.z80.expression.{ExprNumber, NumericExpression}

import scala.language.implicitConversions

case class Variable (name:String) extends Listable {
  override def list: String = name
}

case class ExprIndex(index:List[NumericExpression]) extends Listable {
  def toIndex(environment: Environment):Either[String,Index]={
    val evaluated=index.map(_.evaluate(environment))
    val errors=evaluated.filter(_.isLeft).map(_.swap.toOption.get)
    val correct=evaluated.filter(_.isRight).map(_.toOption.get).map(_.toInt)

    if(errors.isEmpty) Right(Index(correct))
    else Left(errors.mkString(";"))
  }

  override def list: String = if(index.isEmpty) "" else "("+index.map(_.list).mkString(",")+")"
}

object ExprIndex {
  def empty:ExprIndex=new ExprIndex(List())
  def static(staticList:List[Int]):ExprIndex=new ExprIndex(staticList.map(ExprNumber(_)))
}

case class VariableIndex(variable:Variable, index:ExprIndex) extends Listable {
  def length:Int=index.index.length
  def evaluateIndex(environment: Environment):Either[ExitCode,Index] =
    index.toIndex(environment) match {
      case Left(_) => Left(ExitCode.INVALID_ARRAY_INDEX)
      case Right(i) => Right(i)
    }

  override def list: String = f"${variable.list}${index.list}"
}

object VariableIndex {
  def apply(variable: Variable):VariableIndex = new VariableIndex(variable,ExprIndex.empty)
  def apply(name:String,index:ExprIndex):VariableIndex = new VariableIndex(Variable(name),index)
  implicit def fromString(name:String):VariableIndex = VariableIndex(Variable(name))
  def asStatic(variable: Variable, index:Index):VariableIndex =
    new VariableIndex(variable, ExprIndex(index.dimensions.map(dim=>ExprNumber(dim))))
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
  def blank(dimensionsNum: Int):Index = new Index(List.fill(dimensionsNum)(DEFAULT_DIM_SIZE))
}
