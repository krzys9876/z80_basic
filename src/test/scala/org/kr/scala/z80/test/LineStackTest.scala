package org.kr.scala.z80.test

import org.kr.scala.z80.environment.LineStack
import org.kr.scala.z80.program.LineNumber
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class LineStackTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Handle line stack") {
    Scenario("Initialize stack") {
      Given("stack is empty")
      val stack=LineStack.empty
      assert(stack.top.isEmpty)
      When("initial line 10 is pushed")
      val newStack=stack.push(LineNumber(10))
      Then("10 is on top of stack")
      assert(newStack.top.contains(LineNumber(10)))
    }
    Scenario("Push on non-empty stack") {
      Given("stack is not empty")
      val stack=LineStack.empty.push(LineNumber(20))
      assert(stack.top.contains(LineNumber(20)))
      When("new line 30 is pushed")
      val newStack=stack.push(LineNumber(30))
      Then("30 is on top of stack")
      assert(newStack.top.contains(LineNumber(30)))
    }
    Scenario("Pop from non-empty stack") {
      Given("stack has 2 elements (1,2)")
      val stack=LineStack.empty.push(LineNumber(1)).push(LineNumber(2))
      assert(stack.top.contains(LineNumber(2)))
      When("a value is popped from stack")
      val newStack=stack.pop
      Then("1 is on top of stack")
      assert(newStack.top.contains(LineNumber(1)))
      And("after second pop the stack is empty")
      val emptyStack=newStack.pop
      assert(emptyStack.top.isEmpty)
    }
    Scenario("Pop from empty stack") {
      Given("stack is empty")
      val stack=LineStack.empty
      assert(stack.top.isEmpty)
      When("a value is popped from stack")
      val newStack=stack.pop
      Then("stack is still empty")
      assert(newStack.top.isEmpty)
    }
    Scenario("Change top value") {
      Given("stack is not empty")
      val stack=LineStack.empty.push(LineNumber(20)).push(LineNumber(40))
      assert(stack.top.contains(LineNumber(40)))
      When("a top value is changed")
      val newStack=stack.changeTopTo(LineNumber(60))
      Then("top value is changed")
      assert(newStack.top.contains(LineNumber(60)))
      And("after popping from stack the previous value is retained")
      val popStack=newStack.pop
      assert(popStack.top.contains(LineNumber(20)))
    }
  }
}
