package org.kr.scala.z80.environment

import scala.annotation.tailrec

case class Iterator[O](next: O => O, ends: O => Boolean) {
  @tailrec
  final def iterate(o: O): O =
    if (ends(o)) o else iterate(next(o))
}
