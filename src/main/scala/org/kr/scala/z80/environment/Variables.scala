package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{Index, Variable, VariableIndex}
import scala.language.implicitConversions

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