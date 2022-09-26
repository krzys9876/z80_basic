package org.kr.scala.z80.environment

import scala.annotation.tailrec

/**
 * A simple wrapper for step-by-step execution of a program enclosed in an Iterable object
 *
 * Requires an initial instance of an object and a function that signals end of execution (if any)
 * Note: it is a F-bounded type - it is parameterized over its own subtypes
 * to ensure that method 'step' returns parameterized type, not just a simple Iterable
 * see e.g.: https://twitter.github.io/scala_school/advanced-types.html
 *
 *  @param ends A function returning true if a iteration loop should end.
 *              It defaults to false, i.e. never-ending iteration
 *
 *  @tparam O Any type implementing parameterless step method returning a new (changed) version of itself
 *
 */
case class Iterator[O <: Iterable[O]](ends: O => Boolean = (_:Iterable[O]) => false) {
  @tailrec
  final def iterate(o: O): O = if (ends(o)) o else iterate(o.step)
}

/**
 * A trait representing an object that is able to step over and generate a new version of itself
 *
 * @tparam O Any type implementing parameterless step function.
 *           Note: A generic is required to ensure that step returns parameterized type, not a generic Iterable
 */

trait Iterable[O] {
  def step:O
}
