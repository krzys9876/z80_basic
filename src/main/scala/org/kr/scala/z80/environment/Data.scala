package org.kr.scala.z80.environment

case class Data(values:Vector[Any],pos:Int) {
  def read:(Either[ExitCode,Any],Data) = {
    if(pos>=values.length) (Left(ExitCode.OUT_OF_DATA),this)
    else (Right(values(pos)),nextPos)
  }
  def store(valuesToStore:List[Any]):Data = copy(values=values ++ valuesToStore)
  private def nextPos:Data=copy(pos=pos+1)
  def reset:Data=copy(pos=0)
}

object Data {
  def empty:Data=new Data(Vector(),0)
}
