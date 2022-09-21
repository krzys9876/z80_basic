package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{Index, Variable, VariableName, VariableStatic}

import scala.language.implicitConversions

case class Variables(values:Map[VariableStatic,Any], dimensions:Map[VariableName,Index]) {
  def value(variable: VariableName):Option[Any]=values.get(VariableStatic(variable))
  //TODO: convert Option to Either
  def value(variable: Variable, environment: Environment):Either[ExitCode,Any]= {
    variable.evaluateIndex(environment) match {
      case Left(code) => Left(code)
      case Right(index) =>
        checkDimensions(variable.variable) match {
          case Some(dim) if index.fitsSize(dim) =>
            get(Variable.asStatic(variable.variable,index))
          case None => Left(ExitCode.INVALID_ARRAY_INDEX)
          case _ => Left(ExitCode.INVALID_ARRAY_INDEX)
        }
    }
  }

  def store(variable: Variable, valueToStore:Any, environment: Environment):Either[ExitCode,Variables]=
    variable.evaluateIndex(environment) match {
      case Left(_) => Left(ExitCode.INVALID_ARRAY_INDEX)
      case Right(index) =>
        checkDimensions(variable.variable) match {
          case None =>
            Right(storeValue(variable, index, valueToStore)
              .storeDimensions(variable))
          case Some(dim) if index.fitsSize(dim) => Right(storeValue(variable, index, valueToStore))
          case _ => Left(ExitCode.INVALID_ARRAY_INDEX)
        }
    }

  private def get(variableStatic:VariableStatic):Either[ExitCode,Any] =
    values.get(variableStatic) match {
      case None => Left(ExitCode.FATAL_CANNOT_GET_VALUE)
      case Some(v) => Right(v)
    }

  private def storeValue(variable: Variable, evaluatedIndex:Index, valueToStore:Any):Variables=
    copy(values=values ++ Map(Variable.asStatic(variable.variable,evaluatedIndex) -> valueToStore))
  private def storeDimensions(variable: Variable):Variables=
    copy(dimensions=dimensions ++ Map(variable.variable-> Index.blank(variable.length)))
  private def checkDimensions(variable: VariableName):Option[Index] = dimensions.get(variable)
}

object Variables {
  def empty:Variables=new Variables(Map(),Map())
}