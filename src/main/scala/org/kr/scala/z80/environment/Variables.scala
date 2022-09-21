package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{Index, Variable, VariableIndex}
import scala.language.implicitConversions

case class Variables(values:Map[VariableIndex,Any], dimensions:Map[Variable,Index]) {
  def value(variable: Variable):Option[Any]=values.get(VariableIndex(variable))
  //TODO: convert Option to Either
  def value(variableIndex: VariableIndex, environment: Environment):Option[Any]= {
    variableIndex.evaluateIndex(environment) match {
      case Left(_) => None //Left(ExitCode.INVALID_ARRAY_INDEX)
      case Right(index) =>
        checkDimensions(variableIndex.variable) match {
          case None => None //Left(ExitCode.INVALID_ARRAY_INDEX)
          case Some(dim) if index.fitsSize(dim) => values.get(VariableIndex.asStatic(variableIndex.variable,index))
          case _ => None //Left(ExitCode.INVALID_ARRAY_INDEX)
        }
    }
  }

  def store(variableIndex: VariableIndex, valueToStore:Any, environment: Environment):Either[ExitCode,Variables]=
    variableIndex.evaluateIndex(environment) match {
      case Left(_) => Left(ExitCode.INVALID_ARRAY_INDEX)
      case Right(index) =>
        checkDimensions(variableIndex.variable) match {
          case None =>
            Right(storeValue(variableIndex, index, valueToStore)
              .storeDimensions(variableIndex))
          case Some(dim) if index.fitsSize(dim) => Right(storeValue(variableIndex, index, valueToStore))
          case _ => Left(ExitCode.INVALID_ARRAY_INDEX)
        }
    }

  private def storeValue(variable: VariableIndex, evaluatedIndex:Index, valueToStore:Any):Variables=
    copy(values=values ++ Map(VariableIndex.asStatic(variable.variable,evaluatedIndex) -> valueToStore))
  private def storeDimensions(variableIndex: VariableIndex):Variables=
    copy(dimensions=dimensions ++ Map(variableIndex.variable-> Index.blank(variableIndex.length)))
  private def checkDimensions(variable: Variable):Option[Index] = dimensions.get(variable)
}

object Variables {
  def empty:Variables=new Variables(Map(),Map())
}