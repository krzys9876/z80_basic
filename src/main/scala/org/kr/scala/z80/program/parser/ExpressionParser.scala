package org.kr.scala.z80.program.parser

import org.kr.scala.z80.expression.{ExprFunction, ExprNumber, ExprOperation, ExprVariable, NumericExpression}

trait ExpressionParser extends CommonParser with VariableParser {
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


  def expr:PN = factor4

  private def exprParen: PN = "(" ~> expr <~ ")"

  // Factor1 / Operation1 - numbers indicate priority of operations, e.g.: (1) power, (2) multiplication/division, (3) addition/subtraction

  //TODO: add function calls (sin, cos, abs etc.)

  private def factor1:PN = num | func | variableExpr | exprParen
  private def func: PN = ("ABS" | "SIN" | "COS") ~ exprParen ^^ { case name ~ f => ExprFunction(f, name) }
  // Partial operation - decode only operator and the second factor, leave first factor unresolved
  private def operation1:PNN = "^" ~ factor1 ^^ { case op ~ f2 => ExprOperation(_, f2, op) }
  private def neg: PN = "-" ~ factor1 ^^ { case _ ~ f => ExprFunction(f,"-") }
  private def factor2:PN = neg | factor1 ~ rep(operation1) ^^ {case f ~ op => applyOperations(f, op) }
  private def operations2: PNN = ("*" | "/") ~ factor2 ^^ { case oper ~ f2 => ExprOperation(_, f2, oper) }
  private def factor3: PN = factor2 ~ rep(operations2) ^^ {case f ~ op => applyOperations(f, op) }
  private def operations3: PNN = ("+" | "-") ~ factor3 ^^ { case oper ~ b => ExprOperation(_, b, oper) }
  private def factor4: PN = factor3 ~ rep(operations3) ^^ {case f ~ op => applyOperations(f, op) }

  private def num:PN=floatingPointNumber ^^ (d => ExprNumber(d.toDouble))
  private def variableExpr:PN=numVariable ^^ (v => ExprVariable(v))

  // apply all nested operations - resolve first factor in each operation (the second is already resolved)
  private def applyOperations(factor: N, operations: List[N => N]): N =
    operations.foldLeft(factor)((accumulator, operation) => operation(accumulator))
}
