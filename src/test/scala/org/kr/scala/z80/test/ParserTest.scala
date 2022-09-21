package org.kr.scala.z80.test

import org.kr.scala.z80.expression.{BlankTextExpr, ExprNumber, ExprOperation, ExprVariable, StaticTextExpr}
import org.kr.scala.z80.parser.LineParser
import org.kr.scala.z80.program.{Assignment, ExprIndex, FOR, GOSUB, GOTO, IF, Index, LET, Line, NEXT, NumericAssignment, PRINT, PrintableToken, REM, RETURN, Variable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.kr.scala.z80.expression.ExprVariable._
import org.kr.scala.z80.expression.ExprNumber._

class ParserTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("do not parse invalid line") {
    Scenario("do not parse line without number") {
      assert(LineParser("PRINT 'aa'").isLeft)
    }
    Scenario("do not parse invalid statement") {
      assert(LineParser("10 INVALID").isLeft)
    }
  }
  Feature("parse REM line") {
    Scenario("parse REM and comment") {
      assert(LineParser("10 REM aAbB").contains(Line(10, REM("aAbB"))))
      assert(LineParser("20 REM 123 xYz").contains(Line(20, REM("123 xYz"))))
    }
    Scenario("parse REM with quoted comment") {
      assert(LineParser("10 REM 'aA \"bB'").contains(Line(10, REM("'aA \"bB'"))))
      assert(LineParser("20 REM \"aA 'bB\"").contains(Line(20, REM("\"aA 'bB\""))))
      assert(LineParser("30 REM \"\"").contains(Line(30, REM("\"\""))))
      assert(LineParser("40 REM ''").contains(Line(40, REM("''"))))
    }
    Scenario("parse REM without comment") {
      assert(LineParser("10 REM").contains(Line(10,REM(""))))
    }
  }

  Feature("parse PRINT line") {
    Scenario("parse empty PRINT") {
      assert(LineParser("10 PRINT").contains(Line(10,PRINT())))
    }
    Scenario("parse PRINT with single text") {
      assert(LineParser("10 PRINT \"abc d \"").contains(Line(10,PRINT(StaticTextExpr("abc d ")))))
    }
    Scenario("parse PRINT with numeric expression") {
      assert(LineParser("10 PRINT 1+2").contains(Line(10,PRINT(ExprOperation.plus(1,2)))))
    }
    Scenario("parse PRINT with multiple tokens with EOL") {
      assert(LineParser("10 PRINT 1+2,3;\" \"").contains(
        Line(10,PRINT(Vector(
          PrintableToken(ExprOperation.plus(1,2)),
          PrintableToken(Some(","),ExprNumber(3)),
          PrintableToken(Some(";"),StaticTextExpr(" ")))))))
    }
    Scenario("parse PRINT with multiple tokens without EOL") {
      assert(LineParser("10 PRINT A,1;").contains(
        Line(10,PRINT(Vector(
          PrintableToken(ExprVariable("A")),
          PrintableToken(Some(","),ExprNumber(1)),
          PrintableToken(Some(";"),BlankTextExpr))))))
      assert(LineParser("10 PRINT 2,3,").contains(
        Line(10,PRINT(Vector(
          PrintableToken(ExprNumber(2)),
          PrintableToken(Some(","),ExprNumber(3)),
          PrintableToken(Some(","),BlankTextExpr))))))
    }
  }

  Feature("parse NEXT line") {
    Scenario("parse NEXT with variable") {
      assert(LineParser("10 NEXT A").contains(Line(10,NEXT("A"))))
    }
    Scenario("parse NEXT without variable") {
      assert(LineParser("10 NEXT").contains(Line(10,NEXT())))
    }
    Scenario("do not parse NEXT with text variable") {
      assert(LineParser("10 NEXT B$").isLeft)
    }
  }

  Feature("parse LET line for variables") {
    Scenario("parse LET with numeric variable") {
      assert(LineParser("15 LET A=12.34").contains(Line(15,LET(NumericAssignment("A",12.34)))))
      assert(LineParser("15 LET A=12+34").contains(Line(15,
        LET(NumericAssignment("A",ExprOperation.plus(12,34))))))
      assert(LineParser("15 LET A=B*2").contains(Line(15,
        LET(NumericAssignment("A",ExprOperation.mul("B",2))))))
    }
    Scenario("parse assignment without LET keyword") {
      assert(LineParser("15 X=91.82").contains(Line(15,LET(NumericAssignment("X",91.82)))))
    }
    Scenario("parse LET with static text") {
      assert(LineParser("15 LET A$=\"abc\"").contains(Line(15,LET(Assignment("A$",StaticTextExpr("abc"))))))
    }
    Scenario("do not parse mix assignment numeric/text values/variables") {
      assert(LineParser("15 LET A$=123").isLeft)
      assert(LineParser("15 LET A$=B").isLeft)
      assert(LineParser("15 LET A=\"XYZ\"").isLeft)
      assert(LineParser("15 LET A=B$").isLeft)
    }
  }

  Feature("parse LET line for arrays") {
    Scenario("parse LET with numeric array") {
      assert(LineParser("15 LET A(4)=1.2").contains(
        Line(15,LET(NumericAssignment(Variable("A",ExprIndex.static(List(4))),1.2)))))
      assert(LineParser("20 LET B(1,2,3,4,5,6,7)=23.45").contains(
        Line(20,LET(NumericAssignment(Variable("B",ExprIndex.static(List(1,2,3,4,5,6,7))),23.45)))))
      assert(LineParser("25 LET C(1,2)=D(9,3,2)").contains(
        Line(25,LET(NumericAssignment(Variable("C",ExprIndex.static(List(1,2))),ExprVariable(Variable("D",ExprIndex.static(List(9,3,2)))))))))
    }
    Scenario("parse LET with text array") {
      assert(LineParser("15 LET A$(4)=\"abc\"").contains(
        Line(15,LET(Assignment(Variable("A$",ExprIndex.static(List(4))),StaticTextExpr("abc"))))))
      assert(LineParser("20 LET B$(1,2,3,4,5,6,7)=\"cde\"").contains(
        Line(20,LET(Assignment(Variable("B$",ExprIndex.static(List(1,2,3,4,5,6,7))),StaticTextExpr("cde"))))))
      assert(LineParser("25 LET C$(1,2)=D$(4)").contains(
        Line(25,LET(Assignment(Variable("C$",ExprIndex.static(List(1,2))),
          ExprVariable(Variable("D$",ExprIndex.static(List(4)))))))))
    }
    Scenario("do not parse mix assignment numeric/text values/variables/arrays") {
      assert(LineParser("15 LET A$(1)=123").isLeft)
      assert(LineParser("15 LET A$=B(1,8,1)").isLeft)
      assert(LineParser("15 LET A(2,3)=\"XYZ\"").isLeft)
      assert(LineParser("15 LET A=B$(7)").isLeft)
    }
    Scenario("parse expressions in array indexes") {
      assert(LineParser("15 LET A(B+C)=D(E,F)").contains(Line(15,
        LET(NumericAssignment(
          Variable("A",ExprIndex(
            List(ExprOperation.plus(ExprVariable("B"),ExprVariable("C"))))),
          ExprVariable(Variable("D",ExprIndex(List(ExprVariable("E"),ExprVariable("F"))))))))))
      assert(LineParser("15 LET A$(B+C)=D$(E,F)").contains(Line(15,
        LET(Assignment(
          Variable("A$",ExprIndex(
            List(ExprOperation.plus(ExprVariable("B"),ExprVariable("C"))))),
          ExprVariable(Variable("D$",ExprIndex(List(ExprVariable("E"),ExprVariable("F"))))))))))
    }
  }

  Feature("parse FOR line") {
    Scenario("parse FOR without STEP") {
      assert(LineParser("20 FOR I=1 TO 3").contains(Line(20,FOR(NumericAssignment("I",1),3))))
    }
    Scenario("parse FOR with STEP") {
      assert(LineParser("20 FOR I=5 TO 35 STEP 10").contains(Line(20,FOR(NumericAssignment("I",5),35,Some(10)))))
    }
    Scenario("do not parse invalid FOR") {
      assert(LineParser("20 FOR I= TO 35 STEP 10").isLeft)
      assert(LineParser("20 FOR 1=2 TO 35 STEP 10").isLeft)
      assert(LineParser("20 FOR I=2 TO A$").isLeft)
      assert(LineParser("20 FOR I=2 A$").isLeft)
    }
  }

  Feature("parse GOTO line") {
    Scenario("parse GOTO") {
      assert(LineParser("20 GOTO 123").contains(Line(20,GOTO(123))))
    }
    Scenario("do not parse invalid GOTO") {
      assert(LineParser("20 GOTO A").isLeft)
      assert(LineParser("20 GOTO 1+2").isLeft)
      assert(LineParser("20 GOTO").isLeft)
    }
  }
  Feature("parse GOSUB line") {
    Scenario("parse GOSUB") {
      assert(LineParser("20 GOSUB 123").contains(Line(20,GOSUB(123))))
    }
    Scenario("do not parse invalid GOSUB") {
      assert(LineParser("20 GOSUB A").isLeft)
      assert(LineParser("20 GOSUB 1+2").isLeft)
      assert(LineParser("20 GOSUB").isLeft)
    }
  }
  Feature("parse RETURN line") {
    Scenario("parse RETURN") {
      assert(LineParser("30 RETURN").contains(Line(30,RETURN())))
    }
  }
  Feature("parse IF line") {
    Scenario("parse IF") {
      assert(LineParser("20 IF I=5 THEN PRINT \"X\"").contains(Line(20, IF(ExprOperation.eq("I", 5), PRINT(StaticTextExpr("X"))))))
    }
    Scenario("parse variants of IF with GOTO") {
      assert(LineParser("20 IF I=5 THEN GOTO 30").contains(Line(20, IF(ExprOperation.eq("I", 5), GOTO(30)))))
      assert(LineParser("20 IF I=5 GOTO 30").contains(Line(20, IF(ExprOperation.eq("I", 5), GOTO(30)))))
      assert(LineParser("20 IF I=5 THEN 30").contains(Line(20, IF(ExprOperation.eq("I", 5), GOTO(30)))))
    }
  }
}
