package org.kr.scala.z80.test

import org.kr.scala.z80.environment.{ExitCode, LineStack}
import org.kr.scala.z80.program.StatementId
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class LineStackTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Handle line stack") {
    Scenario("Initialize stack") {
      Given("stack is empty")
      val stack=LineStack.empty
      assert(stack.top.isEmpty)
      When("initial line 10 is pushed")
      val newStack=stack.push(StatementId(10))
      Then("10 is on top of stack")
      assert(newStack.top.contains(StatementId(10)))
    }
    Scenario("Push on non-empty stack") {
      Given("stack is not empty")
      val stack=LineStack.empty.push(StatementId(20))
      assert(stack.top.contains(StatementId(20)))
      When("new line 30 is pushed")
      val newStack=stack.push(StatementId(30))
      Then("30 is on top of stack")
      assert(newStack.top.contains(StatementId(30)))
    }
    Scenario("Pop from non-empty stack") {
      Given("stack has 2 elements (1,2)")
      val stack=LineStack.empty.push(StatementId(1)).push(StatementId(2))
      assert(stack.top.contains(StatementId(2)))
      When("a value is popped from stack")
      val newStack=stack.pop.toOption.get
      Then("1 is on top of stack")
      assert(newStack.top.contains(StatementId(1)))
      And("after second pop the stack is empty")
      val emptyStack=newStack.pop
      assert(emptyStack.swap.contains(ExitCode.RETURN_WITHOUT_GOSUB))
    }
    Scenario("Pop from empty stack") {
      Given("stack is empty")
      val stack=LineStack.empty
      assert(stack.top.isEmpty)
      When("a value is popped from stack")
      val newStack=stack.pop
      Then("stack is still empty")
      assert(newStack.swap.contains(ExitCode.RETURN_WITHOUT_GOSUB))
    }
    Scenario("Change top value") {
      Given("stack is not empty")
      val stack=LineStack.empty.push(StatementId(20)).push(StatementId(40))
      assert(stack.top.contains(StatementId(40)))
      When("a top value is changed")
      val newStack=stack.changeTopTo(StatementId(60))
      Then("top value is changed")
      assert(newStack.top.contains(StatementId(60)))
      And("after popping from stack the previous value is retained")
      val popStack=newStack.pop.toOption.get
      assert(popStack.top.contains(StatementId(20)))
    }
  }
}
