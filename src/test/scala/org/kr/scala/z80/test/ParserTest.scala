package org.kr.scala.z80.test

import org.kr.scala.z80.expression.StaticTextExpr
import org.kr.scala.z80.program.{LineParser, Line, LineNumber, PRINT, REM}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ParserTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("parse REM line") {
    Scenario("parse REM and comment") {
      assert(LineParser("10 REM aAbB").contains(Line(LineNumber(10), REM("aAbB"))))
      assert(LineParser("20 REM 123 xYz").contains(Line(LineNumber(20), REM("123 xYz"))))
    }
    Scenario("parse REM with quoted comment") {
      assert(LineParser("10 REM 'aA \"bB'").contains(Line(LineNumber(10), REM("aA \"bB"))))
      assert(LineParser("20 REM \"aA 'bB\"").contains(Line(LineNumber(20), REM("aA 'bB"))))
      assert(LineParser("30 REM \"\"").contains(Line(LineNumber(30), REM(""))))
      assert(LineParser("40 REM ''").contains(Line(LineNumber(40), REM(""))))
    }
    Scenario("parse REM without comment") {
      assert(LineParser("10 REM").contains(Line(LineNumber(10),REM(""))))
    }
  }

  Feature("parse PRINT line") {
    Scenario("parse PRINT with single text") {
      assert(LineParser("10 PRINT \"abc d \"").contains(Line(LineNumber(10),PRINT(StaticTextExpr("abc d ")))))
    }
  }

  Feature("parse invalid line") {
    Scenario("parse invalid line") {
      assert(LineParser("PRINT 'aa'").isLeft)
    }
    Scenario("parse invalid statement") {
      assert(LineParser("10 INVALID").isLeft)
    }
  }
}
