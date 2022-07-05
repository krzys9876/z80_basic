package org.kr.scala.z80.test

import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ProgramTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Run program line by line") {
    Scenario("Run only print lines") {
      Given("a program consisting only print lines")
    }
  }
}
