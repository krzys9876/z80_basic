package org.kr.scala.z80.program.parser

import org.kr.scala.z80.expression.{ExprFunction, ExprNumber, ExprOperation, ExprVariable, NumericExpression}

trait NumericExpressionParser extends CommonParser with VariableParser {
  // Output type
  type N=NumericExpression
  // Parser of the output type
  type PN=Parser[N]
  // Operation: a function from expression to expression. It is a partial function that is parsed - only operator and second factor
  // The first factor is resolved from previous chain of operations. This allows creation of nested objects.
  // e.g. "1 + 2 + 3" creates nested structure:
  // ExprOperation(
  //   ExprOperation(
  //     ExprNumber(1),
  //     ExprNumber(2),
  //     "+"),
  //   ExprNumber(3),
  //   "+")
  type PNN=Parser[N=>N]

  // result
  def numericExpression:PN = factor8

  // FactorX / OperationX - numbers indicate order of precedence of operations
  // Precedence is defined in MS Basic documentation (page 5.4)
  private def factor1:PN = num | func | variableExpr | exprParen
  // Partial operation - decode only operator and the second factor, leave first factor unresolved
  private def operation1:PNN = power ~ factor1 ^^ { case op ~ f2 => ExprOperation(_, f2, op) }
  private def factor2:PN = neg | factor1 ~ rep(operation1) ^^ {case f ~ op => applyOperations(f, op) }
  private def operations2: PNN = multiplication ~ factor2 ^^ { case oper ~ f2 => ExprOperation(_, f2, oper) }
  private def factor3: PN = factor2 ~ rep(operations2) ^^ {case f ~ op => applyOperations(f, op) }
  private def operations3: PNN = addition ~ factor3 ^^ { case oper ~ f => ExprOperation(_, f, oper) }
  private def factor4: PN = factor3 ~ rep(operations3) ^^ {case f ~ op => applyOperations(f, op) }
  private def operations4: PNN = relational ~ factor4 ^^ { case oper ~ f => ExprOperation(_, f, oper) }
  private def factor5: PN = factor4 ~ rep(operations4) ^^ {case f ~ op => applyOperations(f, op) }
  private def operations5: PN = logicNot ~ factor5 ^^ { case name ~ f => ExprFunction(f,name) }
  private def factor6: PN = operations5 | factor5
  private def operations6: PNN = logicAnd ~ factor6 ^^ { case oper ~ f => ExprOperation(_, f, oper) }
  private def factor7: PN = factor6 ~ rep(operations6) ^^ {case f ~ op => applyOperations(f, op) }
  private def operations7: PNN = logicOr ~ factor7 ^^ { case oper ~ f => ExprOperation(_, f, oper) }
  private def factor8: PN = factor7 ~ rep(operations7) ^^ {case f ~ op => applyOperations(f, op) }

  //Building blocks for hierarchy of operations
  private def num:PN=floatingPointNumber ^^ (d => ExprNumber(d.toDouble))
  private def variableExpr:PN=numVariable ^^ (v => ExprVariable(v))
  private def exprParen: PN = "(" ~> numericExpression <~ ")"
  private def func: PN = ("ABS" | "SIN" | "COS") ~ exprParen ^^ { case name ~ f => ExprFunction(f, name) }
  private def neg: PN = "-" ~ factor1 ^^ { case _ ~ f => ExprFunction(f,"-") }
  private def power:Parser[String] = "^"
  private def multiplication:Parser[String] = "*" | "/"
  private def addition:Parser[String] = "+" | "-"
  private def relational:Parser[String] = "<>" | ">=" | ">" | "<=" | "<" | "="
  //NOTE: logical operations (not, and, or) are just bitwise operators.
  // Since false is represented by 0 (all bits set to 0) and true is represented by -1 (all 16 bits set to 1)
  // all three functions work the same on numbers and semi-boolean values 0 and -1
  private def logicNot:Parser[String] = "NOT"
  private def logicAnd:Parser[String] = "AND"
  private def logicOr:Parser[String] = "OR"

  // apply all nested operations - resolve first factor in each operation (the second is already resolved)
  private def applyOperations(factor: N, operations: List[N => N]): N =
    operations.foldLeft(factor)((accumulator, operation) => operation(accumulator))
}
