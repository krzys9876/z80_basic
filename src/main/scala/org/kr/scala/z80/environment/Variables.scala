package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{Index, Variable, VariableName, VariableStatic}

import scala.language.implicitConversions

case class Variables(values:Map[VariableStatic,Any], dimensions:Map[VariableName,Index]) {
  def value(variable: Variable, environment: Environment):Either[ExitCode,Any]= {
    variable.evaluateIndex(environment) match {
      case Left(code) => Left(code)
      case Right(index) =>
        checkDimensions(variable.name) match {
          // get a value of a variable that was previously stored
          case Some(dim) if index.fitsSize(dim) => Right(get(variable,index))
          // get a default value
          //NOTE: it is possible to get a default value from array of the same name but different dimensions
          //but this is a minor issue and resolving it would require that reading a value from uninitialized variable
          // would require environment to change (add dimension)
          case None => Right(get(variable,index))
          // invalid index of previously stored array
          case Some(_) => Left(ExitCode.INVALID_ARRAY_INDEX)
        }
    }
  }

  def store(variable: Variable, valueToStore:Any, environment: Environment):Either[ExitCode,Variables]=
    variable.evaluateIndex(environment) match {
      case Left(_) => Left(ExitCode.INVALID_ARRAY_INDEX)
      case Right(index) =>
        checkDimensions(variable.name) match {
          case None =>
            Right(storeValue(variable, index, valueToStore)
              .storeDimensions(variable))
          case Some(dim) if index.fitsSize(dim) => Right(storeValue(variable, index, valueToStore))
          case _ => Left(ExitCode.INVALID_ARRAY_INDEX)
        }
    }
  def setArrayDim(variableStatic: VariableStatic):Variables =
    copy(dimensions = dimensions ++ Map(variableStatic.variableName->variableStatic.index) )

  private def get(variable:Variable,index:Index):Any =
    values.getOrElse(variable.asStatic(index), variable.defaultValue)
  private def storeValue(variable: Variable, evaluatedIndex:Index, valueToStore:Any):Variables=
    copy(values=values ++ Map(variable.asStatic(evaluatedIndex) -> valueToStore))
  private def storeDimensions(variable: Variable):Variables=
    copy(dimensions=dimensions ++ Map(variable.name-> Index.blank(variable.length)))
  private def checkDimensions(variable: VariableName):Option[Index] = dimensions.get(variable)
}

object Variables {
  def empty:Variables=new Variables(Map(),Map())
}