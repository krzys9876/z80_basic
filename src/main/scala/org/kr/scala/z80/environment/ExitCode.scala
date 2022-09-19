package org.kr.scala.z80.environment

sealed trait ExitCode

object ExitCode {
  case object NORMAL extends ExitCode {override def toString: String = "NORMAL"}
  case object PROGRAM_END extends ExitCode {override def toString: String = "PROGRAM_END"}
  case object INVALID_LINE extends ExitCode {override def toString: String = "INVALID_LINE"}
  case object FATAL_LINE_NOT_FOUND extends ExitCode {override def toString: String = "FATAL_LINE_NOT_FOUND"}
  case object FATAL_FOR_MISSING_VALUE extends ExitCode {override def toString: String = "FATAL_FOR_MISSING_VALUE"}
  case object FATAL_IF_INVALID_CONDITION extends ExitCode {override def toString: String = "FATAL_IF_INVALID_CONDITION"}
  case object FATAL_FOR_CANNOT_GET_VALUE extends ExitCode {override def toString: String = "FATAL_FOR_CANNOT_GET_VALUE"}
  case object MISSING_NEXT extends ExitCode {override def toString: String = "MISSING_NEXT"}
  case object MISSING_FOR extends ExitCode {override def toString: String = "MISSING_FOR"}
  case object MISSING_RETURN_LINE extends ExitCode {override def toString: String = "MISSING_RETURN_LINE"}
}