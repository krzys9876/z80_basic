package org.kr.scala.z80.program

import org.kr.scala.z80.environment.{Environment, ExitCode}
import org.kr.scala.z80.expression.{ExprNumber, NumericExpression}

import scala.language.implicitConversions

case class Variable(name:VariableName, index:ExprIndex) extends Listable {
  def length:Int=index.num.length
  def evaluateIndex(environment: Environment):Either[ExitCode,Index] =
    index.toIndex(environment) match {
      case Left(_) => Left(ExitCode.INVALID_ARRAY_INDEX)
      case Right(i) => Right(i)
    }
  def asStatic(index:Index):VariableStatic = new VariableStatic(name, Index(index.dimensions))
  def defaultValue:Any=if(name.isText) "" else BigDecimal(0)

  override def list: String = f"${name.list}${index.list}"
}

object Variable {
  def apply(variable: VariableName):Variable = new Variable(variable,ExprIndex.empty)
  def apply(name:String,index:ExprIndex):Variable = new Variable(VariableName(name),index)
  implicit def fromString(name:String):Variable = Variable(VariableName(name))
}

case class VariableName(name:String) extends Listable {
  def isNumeric:Boolean= !isText
  def isText:Boolean=name.endsWith("$")
  override def list: String = name
}

case class VariableStatic(variableName: VariableName, index: Index) extends Listable {
  override def list: String = f"${variableName.list}${index.list}"
}

object VariableStatic {
  def apply(variableName: VariableName):VariableStatic=new VariableStatic(variableName,Index.empty)
}

/** Encapsulates a list of numeric expressions representing dynamic index of an array
 * It is used in a program definition
 */
case class ExprIndex(num:List[NumericExpression]) extends Listable {
  def toIndex(environment: Environment):Either[String,Index]={
    val evaluated=num.map(_.evaluate(environment))
    val errors=evaluated.filter(_.isLeft).map(_.swap.toOption.get)
    val correct=evaluated.filter(_.isRight).map(_.toOption.get).map(_.toInt)

    if(errors.isEmpty) Right(Index(correct))
    else Left(errors.mkString(";"))
  }

  override def list: String = if(num.isEmpty) "" else "("+num.map(_.list).mkString(",")+")"
}

object ExprIndex {
  def empty:ExprIndex=new ExprIndex(List())
  def static(staticList:List[Int]):ExprIndex=new ExprIndex(staticList.map(ExprNumber(_)))
}

/** Joins a variable name with its index (empty for simple variables, defined for arrays).
 * Index consists of a list of numeric expressions to enable dynamic array reference.
 */

/** Contains a list of numbers representing size of ech dimension.
 * E.g.: (5,10) means first dimension is 5-elements long, second dimension is 10 elements long.
 * According to MS Basic documentation the default dimension size is 10, if not specified by DIM statement
 */
case class Index(dimensions: List[Int]) extends Listable {
  def fitsSize(sizeIndex: Index):Boolean = {
    sameLength(sizeIndex) && indexLessThanSize(sizeIndex)
  }
  private def sameLength(sizeIndex: Index):Boolean = length==sizeIndex.length
  // According to documetation - minimum index=0, maximum is set by DIM (or default=10), hence >= not >
  private def indexLessThanSize(sizeIndex: Index):Boolean =
    dimensions.zip(sizeIndex.dimensions).forall({case(index,size)=> size>=index && index>=0})

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
