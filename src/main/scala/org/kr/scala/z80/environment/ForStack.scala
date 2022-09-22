package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{StatementId, Variable}

case class ForState(variable:Variable,
                    start:BigDecimal, end:BigDecimal, step:BigDecimal,
                    forLine:StatementId, status:ForStatus)

object ForState {
  def apply(variable:Variable, start:BigDecimal, end:BigDecimal, step:BigDecimal, forLine:StatementId):ForState=
    new ForState(variable,start,end,step,forLine,ForStatus.STARTED)
  def apply(variable:Variable, start:BigDecimal, end:BigDecimal, forLine:StatementId):ForState=
    new ForState(variable,start,end,1,forLine,ForStatus.STARTED)
  def apply(name:String,start:BigDecimal,end:BigDecimal,forLine:StatementId,status:ForStatus):ForState=
    new ForState(Variable.fromString(name),start,end,1,forLine,status)
}

class ForStack(private val map:Map[Variable,ForState]) {
  def isEmpty:Boolean=map.isEmpty
  def push(variable:Variable, state:ForState):ForStack=new ForStack(map ++ Map(variable->state))
  def pop(variable:Variable):ForStack=new ForStack(map.removed(variable))

  // find 'for' statement for a given variable of any 'for' before given line number
  def lineFor(variable:Variable):Option[ForState]= map.get(variable)
  def lineFor(beforeLineNum:StatementId):Option[ForState]= findLineBefore(beforeLineNum)

  private def findLineBefore(beforeNum: StatementId):Option[ForState]={
    map.values.toList.filter(_.forLine.isBefore(beforeNum))
      .foldLeft(Option.empty[ForState])(
        (returnLine, state) =>
          returnLine match {
            // first pass
            case None => Some(state)
            // another pass
            case Some(accumulatedState) =>
              if (accumulatedState.forLine.isBefore(state.forLine)) Some(state) else Some(accumulatedState)
          }
      )
  }
}

object ForStack {
  def empty:ForStack=new ForStack(Map())
}

sealed trait ForStatus

object ForStatus {
  case object STARTED extends ForStatus
  case object FINISHED extends ForStatus
}
