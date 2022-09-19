package org.kr.scala.z80.test

import org.kr.scala.z80.expression.{ExprNumber, ExprOperation, StaticTextExpr}
import org.kr.scala.z80.program.parser.LineParser
import org.kr.scala.z80.program.{Line, LineNumber, NEXT, PRINT, REM, Variable}
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
    Scenario("parse PRINT with numeric expression") {
      assert(LineParser("10 PRINT 1+2").contains(Line(LineNumber(10),PRINT(ExprOperation(ExprNumber(1),ExprNumber(2),"+")))))
    }
  }

  Feature("parse NEXT line") {
    Scenario("parse NEXT with variable") {
      assert(LineParser("10 NEXT A").contains(Line(LineNumber(10),NEXT(Some(Variable("A"))))))
    }
    Scenario("parse NEXT without variable") {
      assert(LineParser("10 NEXT").contains(Line(LineNumber(10),NEXT(None))))
    }
    Scenario("parse NEXT with text variable (invalid)") {
      assert(LineParser("10 NEXT B$").isLeft)
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
