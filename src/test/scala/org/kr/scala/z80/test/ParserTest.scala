package org.kr.scala.z80.test

import org.kr.scala.z80.expression.{BlankTextExpr, ExprNumber, ExprOperation, ExprVariable, StaticTextExpr, TextExprVariable}
import org.kr.scala.z80.parser.LineParser
import org.kr.scala.z80.program.{Assignment, DATA, DIM, ExprIndex, FOR, GOSUB, GOTO, IF, Index, LET, Line, NEXT, NumericAssignment, PRINT, PrintableToken, READ, REM, RETURN, STOP, Variable, VariableName, VariableStatic}
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
    Scenario("print text array") {
      assert(LineParser("20 PRINT A$(B(C))").contains(
        Line(20,PRINT(TextExprVariable(Variable("A$",ExprIndex(List(ExprVariable(Variable("B",ExprIndex(List(ExprVariable("C")))))))))))))
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
    Scenario("parse numeric array in nested indexes") {
      assert(LineParser("20 LET A(B(C(D)))=E(F(G))").contains(Line(20,
        LET(NumericAssignment(Variable("A",ExprIndex(
            List(ExprVariable(Variable("B",ExprIndex(
              List(ExprVariable(Variable("C",ExprIndex(
                List(ExprVariable(Variable("D"))))))))))))),
          ExprVariable(Variable("E",ExprIndex(
            List(ExprVariable(Variable("F",ExprIndex(
              List(ExprVariable("G"))))))))))))))
    }
    Scenario("parse text array in nested indexes") {
      assert(LineParser("20 LET A$(B(C(D)))=E$(F(G))").contains(Line(20,
        LET(Assignment(Variable("A$",ExprIndex(
          List(ExprVariable(Variable("B",ExprIndex(
            List(ExprVariable(Variable("C",ExprIndex(
              List(ExprVariable(Variable("D"))))))))))))),
          ExprVariable(Variable("E$",ExprIndex(
            List(ExprVariable(Variable("F",ExprIndex(
              List(ExprVariable("G"))))))))))))))
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
  Feature("parse DIM line") {
    Scenario("parse DIM with static index") {
      assert(LineParser("10 DIM A(20,30)").contains(Line(10,DIM(Variable(VariableName("A"),ExprIndex.static(List(20,30)))))))
      assert(LineParser("20 DIM B(2)").contains(Line(20,DIM(Variable(VariableName("B"),ExprIndex.static(List(2)))))))
      assert(LineParser("30 DIM C(10,20,30,40,50,60)").contains(
        Line(30,DIM(Variable(VariableName("C"),ExprIndex.static(List(10,20,30,40,50,60)))))))
    }
    Scenario("parse DIM with dynamic index") {
      assert(LineParser("10 DIM A(B+1)").contains(Line(10,DIM(Variable(VariableName("A"),ExprIndex(List(ExprOperation.plus(ExprVariable("B"),ExprNumber(1)))))))))
    }
  }
  Feature("parse DATA line") {
    Scenario("parse DATA") {
      assert(LineParser("10 DATA 1, \"q , we\" ,2.3, abc, 13.1").contains(Line(10,DATA(List(1,"q , we",2.3,"abc",13.1)))))
    }
  }
  Feature("parse READ line") {
    Scenario("parse READ") {
      assert(LineParser("10 READ A,B,C$").contains(Line(10,READ(List("A","B","C$")))))
      assert(LineParser("10 READ B$").contains(Line(10,READ(List("B$")))))
      assert(LineParser("10 READ C(1,D),E$(F,2)").contains(
        Line(10,READ(List(
          Variable(VariableName("C"),ExprIndex(List(ExprNumber(1),ExprVariable("D")))),
          Variable(VariableName("E$"),ExprIndex(List(ExprVariable("F"),ExprNumber(2)))))))))
      assert(LineParser("10 READ G$(H,I)").contains(
        Line(10,READ(List(Variable(VariableName("G$"),ExprIndex(List(ExprVariable("H"),ExprVariable("I")))))))))
    }
  }
  Feature("parse STOP line") {
    Scenario("parse STOP") {
      assert(LineParser("10 STOP").contains(Line(10,STOP())))
    }
  }
  Feature("parse multiple statements in one line") {
    Scenario("parse multiple statements") {
      assert(LineParser("10 A=1:PRINT A:B=2:PRINT B").contains(
        Line(10,Vector(
          LET(NumericAssignment("A",ExprNumber(1))),PRINT("A"),
          LET(NumericAssignment("B",ExprNumber(2))),PRINT("B")))))
      assert(LineParser("10 A=1:FOR I=1 TO 5:A=A+I:NEXT I").contains(
        Line(10,Vector(
          LET(NumericAssignment("A",ExprNumber(1))),
          FOR(NumericAssignment("I",ExprNumber(1)),ExprNumber(5)),
          LET(NumericAssignment("A",ExprOperation.plus(ExprVariable("A"),ExprVariable("I")))),
          NEXT("I")))))
    }
  }
}
