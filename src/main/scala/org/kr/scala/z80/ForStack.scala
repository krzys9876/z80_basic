package org.kr.scala.z80

case class ForState(variable:Variable, forLine:LineNumber, status:ForStatus)

object ForState {
  def apply(variable:Variable, forLine:LineNumber):ForState=new ForState(variable,forLine,ForStatus.STARTED)
}

class ForStack(private val map:Map[Variable,ForState]) {
  def isEmpty:Boolean=map.isEmpty
  def push(variable:Variable,state:ForState):ForStack=new ForStack(map ++ Map(variable->state))
  def pop(variable:Variable):ForStack=new ForStack(map.removed(variable))

  // find 'for' statement for a given variable of any 'for' before given line number
  def lineFor(variable:Variable):Option[ForState]= map.get(variable)
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
