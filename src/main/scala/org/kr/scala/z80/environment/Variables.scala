package org.kr.scala.z80.environment

import org.kr.scala.z80.program.Variable

case class Variables(values:Map[VariableIndex,Any], dimensions:Map[Variable,Index]) {
  def value(variable: Variable):Option[Any]=values.get(VariableIndex(variable))
  def value(variableIndex: VariableIndex):Option[Any]=values.get(variableIndex)

  def store(variable: Variable, valueToStore:Any):Either[ExitCode,Variables]= {
    checkDimensions(variable) match {
      case Some(d) if d==Index.empty =>Right(storeValue(VariableIndex(variable),valueToStore))
      case None =>Right(storeValue(VariableIndex(variable),valueToStore))
      case _ =>Left(ExitCode.INVALID_ARRAY_INDEX)
    }
  }
  def store(variableIndex: VariableIndex, valueToStore:Any):Either[ExitCode,Variables]= {
    checkDimensions(variableIndex.variable) match {
      case None =>
        Right(storeValue(variableIndex,valueToStore)
          .storeDimensions(variableIndex))
      case Some(d) if variableIndex.index.fitsSize(d) => Right(storeValue(variableIndex,valueToStore))
      case _=>Left(ExitCode.INVALID_ARRAY_INDEX)
    }
  }
  private def storeValue(variable: VariableIndex, valueToStore:Any):Variables=
    copy(values=values ++ Map(variable->valueToStore))
  private def storeDimensions(variableIndex: VariableIndex):Variables=
    copy(dimensions=dimensions ++ Map(variableIndex.variable-> Index.blank(variableIndex.index)))
  private def checkDimensions(variable: Variable):Option[Index] = dimensions.get(variable)
}

object Variables {
  def empty:Variables=new Variables(Map(),Map())
}

case class VariableIndex(variable:Variable, index:Index)

object VariableIndex {
  def apply(variable: Variable):VariableIndex = new VariableIndex(variable,Index.empty)
}

// Contains a list of numbers representing size of ech dimension.
// E.g.: (5,10) means first dimension is 5-elements long, second dimension is 10 elements long.
// According to MS Basic documentation the default dimension size is 10, if not specified by DIM statement
case class Index(dimensions: List[Int]) {
  def fitsSize(sizeIndex: Index):Boolean = {
    sameLength(sizeIndex) && indexLessThanSize(sizeIndex)
  }
  private def sameLength(sizeIndex: Index):Boolean = length==sizeIndex.length
  private def indexLessThanSize(sizeIndex: Index):Boolean =
    dimensions.zip(sizeIndex.dimensions).forall({case(index,size)=>size>index && index>=0})

  def length:Int=dimensions.length
}

object Index {
  val DEFAULT_DIM_SIZE:Int=10
  def empty:Index = new Index(List())
  def blank(dimensions: Index):Index = {
    val dimLen=dimensions.length
    new Index(List.fill(dimLen)(DEFAULT_DIM_SIZE))
  }
}