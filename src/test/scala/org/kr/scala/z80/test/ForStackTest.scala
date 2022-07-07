package org.kr.scala.z80.test

import org.kr.scala.z80.{ForStack, LineNumber}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ForStackTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Handle FOR stack") {
    Scenario("Initialize stack") {
      Given("stack is empty")
      val stack=ForStack.empty
      assert(stack.isEmpty)
      When("initial variable I and line 10 is pushed")
      val newStack=stack.push("I",LineNumber(10))
      Then("stack contains line 10 for variable I")
      assert(newStack.lineFor("I").contains(LineNumber(10)))
    }
    Scenario("Push on non-empty stack") {
      Given("stack is not empty")
      val stack=ForStack.empty.push("I",LineNumber(20)).push("J",LineNumber(50))
      assert(stack.lineFor("I").contains(LineNumber(20)))
      assert(stack.lineFor("J").contains(LineNumber(50)))
      When("new variable K and line 100 is pushed")
      val newStack=stack.push("K",LineNumber(100))
      Then("stack contains line 100 for variable K")
      assert(newStack.lineFor("K").contains(LineNumber(100)))
    }
    Scenario("Pop from non-empty stack") {
      Given("stack has 2 variables (G,H)")
      val stack=ForStack.empty.push("G",LineNumber(1)).push("H",LineNumber(2))
      assert(stack.lineFor("G").contains(LineNumber(1)))
      assert(stack.lineFor("H").contains(LineNumber(2)))
      When("variable G is popped from stack")
      val newStack=stack.pop("G")
      Then("stack contains line 2 for vatiable H")
      assert(newStack.lineFor("H").contains(LineNumber(2)))
      And("stack does not contain line for variable G")
      assert(newStack.lineFor("H").contains(LineNumber(2)))
      And("after removal of variable H the stack is empty")
      val emptyStack=newStack.pop("H")
      assert(emptyStack.isEmpty)
    }
    Scenario("Pop from empty stack") {
      Given("stack is empty")
      val stack=ForStack.empty
      assert(stack.isEmpty)
      When("a non-existing variable is removed from stack")
      val newStack=stack.pop("A")
      Then("stack is still empty")
      assert(newStack.isEmpty)
    }
    Scenario("Pop of non-existing variable from non-empty stack") {
      Given("stack is not empty")
      val stack=ForStack.empty.push("W",LineNumber(20))
      assert(stack.lineFor("W").contains(LineNumber(20)))
      When("a non-existing variable is checked")
      assert(stack.lineFor("X").isEmpty)
      Then("stack returns empty value")
    }
  }
}
