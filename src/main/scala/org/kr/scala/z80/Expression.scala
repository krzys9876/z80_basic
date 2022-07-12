package org.kr.scala.z80

abstract class Expression extends Listable {
  override def list: String =
    result match {
      case s: String => s
      case n: BigDecimal => n.toString()
      case n: Int => n.toString
      case n: Long => n.toString
      case n: Double => n.toString
      case b: Boolean => b.toString
      case _ => "TYPE NOT SUPPORTED"
    }

  val result: Any
  val resultNum: Option[BigDecimal]
  val resultText: Option[String]
}

case class Result(value: Any) extends Expression {
  override val result: Any = value
  override val resultNum: Option[BigDecimal] =
    value match {
      case n: BigDecimal => Some(n)
      case _ => None
    }
  override val resultText: Option[String] =
    value match {
      case s: String => Some(s)
      case _ => None
    }

  override def list: String = resultText.getOrElse(resultNum.map(_.toString).getOrElse("EMPTY"))
}

object Result {
  def apply(value: Any): Result = {
    val valueTyped = value match {
      case n: Int => BigDecimal(n)
      case n: Long => BigDecimal(n)
      case n: Double => BigDecimal(n)
      case n: BigDecimal => n
      case b: Boolean => BigDecimal(if (b) 1 else 0)
      case s: String => s
    }
    new Result(valueTyped)
  }
}
