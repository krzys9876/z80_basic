package org.kr.scala.z80.environment

import org.kr.scala.z80.program.{LineNumber, VariableIndex}

case class ForState(variableIndex:VariableIndex,
                    start:BigDecimal,end:BigDecimal,step:BigDecimal,
                    forLine:LineNumber, status:ForStatus)

object ForState {
  def apply(variableIndex:VariableIndex,start:BigDecimal,end:BigDecimal,step:BigDecimal,forLine:LineNumber):ForState=
    new ForState(variableIndex,start,end,step,forLine,ForStatus.STARTED)
  def apply(variableIndex:VariableIndex,start:BigDecimal,end:BigDecimal,forLine:LineNumber):ForState=
    new ForState(variableIndex,start,end,1,forLine,ForStatus.STARTED)
  def apply(name:String,start:BigDecimal,end:BigDecimal,forLine:LineNumber,status:ForStatus):ForState=
    new ForState(VariableIndex.fromString(name),start,end,1,forLine,status)
}

class ForStack(private val map:Map[VariableIndex,ForState]) {
  def isEmpty:Boolean=map.isEmpty
  def push(variableIndex:VariableIndex,state:ForState):ForStack=new ForStack(map ++ Map(variableIndex->state))
  def pop(variableIndex:VariableIndex):ForStack=new ForStack(map.removed(variableIndex))

  // find 'for' statement for a given variable of any 'for' before given line number
  def lineFor(variableIndex:VariableIndex):Option[ForState]= map.get(variableIndex)
  def lineFor(beforeLineNum:LineNumber):Option[ForState]= findLineBefore(beforeLineNum)

  private def findLineBefore(beforeNum: LineNumber):Option[ForState]={
    map.values.toList.filter(_.forLine.num<beforeNum.num)
      .foldLeft(Option.empty[ForState])(
        (returnLine, state) =>
          returnLine match {
            // first pass
            case None => Some(state)
            // another pass
            case Some(accumulatedState) =>
              if (state.forLine.num > accumulatedState.forLine.num) Some(state) else Some(accumulatedState)
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
