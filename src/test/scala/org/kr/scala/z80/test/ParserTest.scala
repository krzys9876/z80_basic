package org.kr.scala.z80.test

import org.kr.scala.z80.expression.{ExprOperation, ExprVariable, StaticTextExpr}
import org.kr.scala.z80.program.parser.LineParser
import org.kr.scala.z80.program.{FOR, LET, Line, NEXT, NumericAssignment, PRINT, REM}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.program.Variable._
import org.kr.scala.z80.expression.ExprVariable._
import org.kr.scala.z80.expression.ExprNumber._

class ParserTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("parse REM line") {
    Scenario("parse REM and comment") {
      assert(LineParser("10 REM aAbB").contains(Line(10, REM("aAbB"))))
      assert(LineParser("20 REM 123 xYz").contains(Line(20, REM("123 xYz"))))
    }
    Scenario("parse REM with quoted comment") {
      assert(LineParser("10 REM 'aA \"bB'").contains(Line(10, REM("aA \"bB"))))
      assert(LineParser("20 REM \"aA 'bB\"").contains(Line(20, REM("aA 'bB"))))
      assert(LineParser("30 REM \"\"").contains(Line(30, REM(""))))
      assert(LineParser("40 REM ''").contains(Line(40, REM(""))))
    }
    Scenario("parse REM without comment") {
      assert(LineParser("10 REM").contains(Line(10,REM(""))))
    }
  }

  Feature("parse PRINT line") {
    Scenario("parse PRINT with single text") {
      assert(LineParser("10 PRINT \"abc d \"").contains(Line(10,PRINT(StaticTextExpr("abc d ")))))
    }
    Scenario("parse PRINT with numeric expression") {
      assert(LineParser("10 PRINT 1+2").contains(Line(10,PRINT(ExprOperation.plus(1,2)))))
    }
  }

  Feature("parse NEXT line") {
    Scenario("parse NEXT with variable") {
      assert(LineParser("10 NEXT A").contains(Line(10,NEXT("A"))))
    }
    Scenario("parse NEXT without variable") {
      assert(LineParser("10 NEXT").contains(Line(10,NEXT())))
    }
    Scenario("parse NEXT with text variable (invalid)") {
      assert(LineParser("10 NEXT B$").isLeft)
    }
  }

  Feature("parse LET line ") {
    Scenario("parse LET with numeric variable") {
      assert(LineParser("15 LET A=12.34").contains(Line(15,LET(NumericAssignment("A",12.34)))))
      assert(LineParser("15 LET A=12+34").contains(Line(15,
        LET(NumericAssignment("A",ExprOperation.plus(12,34))))))
      assert(LineParser("15 LET A=B*2").contains(Line(15,
        LET(NumericAssignment("A",ExprOperation.mul(ExprVariable("B"),2))))))
    }
  }

  Feature("parse FOR line") {
    Scenario("parse FOR without STEP") {
      assert(LineParser("20 FOR I=1 TO 3").contains(Line(20,FOR(NumericAssignment("I",1),3))))
    }
    Scenario("parse FOR with STEP") {
      assert(LineParser("20 FOR I=5 TO 35 STEP 10").contains(Line(20,FOR(NumericAssignment("I",5),35,Some(10)))))
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
