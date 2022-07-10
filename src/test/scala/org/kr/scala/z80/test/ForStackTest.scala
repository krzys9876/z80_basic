package org.kr.scala.z80.test

import org.kr.scala.z80.{ForStack, ForState, ForStatus, LineNumber, Variable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ForStackTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Handle FOR stack") {
    Scenario("Initialize stack") {
      Given("stack is empty")
      val stack=ForStack.empty
      assert(stack.isEmpty)
      When("initial variable I and line 10 is pushed")
      val newStack=stack.push(Variable("I"),ForState(Variable("I"),LineNumber(10)))
      Then("stack contains line 10 for variable I")
      assert(newStack.lineFor(Variable("I")).contains(ForState(Variable("I"),LineNumber(10))))
    }
    Scenario("Push to non-empty stack") {
      Given("stack is not empty")
      val stack=ForStack.empty
        .push(Variable("I"),ForState(Variable("I"),LineNumber(20)))
        .push(Variable("J"),ForState(Variable("J"),LineNumber(50)))
      assert(stack.lineFor(Variable("I")).contains(ForState(Variable("I"),LineNumber(20))))
      assert(stack.lineFor(Variable("J")).contains(ForState(Variable("J"),LineNumber(50))))
      When("new variable K and line 100 is pushed")
      val newStack=stack.push(Variable("K"),ForState(Variable("K"),LineNumber(100)))
      Then("stack contains line 100 for variable K")
      assert(newStack.lineFor(Variable("K")).contains(ForState(Variable("K"),LineNumber(100))))
    }
    Scenario("Change for loop status for non-empty stack") {
      Given("stack is not empty")
      val stack=ForStack.empty
        .push(Variable("I"),ForState(Variable("I"),LineNumber(10)))
        .push(Variable("J"),ForState(Variable("J"),LineNumber(20)))
      assert(stack.lineFor(Variable("I")).contains(ForState(Variable("I"),LineNumber(10),ForStatus.STARTED)))
      assert(stack.lineFor(Variable("J")).contains(ForState(Variable("J"),LineNumber(20),ForStatus.STARTED)))
      When("existing variable with changed status is pushed")
      val newStack=stack.push(Variable("I"),ForState(Variable("I"),LineNumber(10),ForStatus.FINISHED))
      Then("stack contains changed status")
      assert(newStack.lineFor(Variable("I")).contains(ForState(Variable("I"),LineNumber(10),ForStatus.FINISHED)))
      And("other elements remain unchanged")
      assert(stack.lineFor(Variable("J")).contains(ForState(Variable("J"),LineNumber(20),ForStatus.STARTED)))
    }
    Scenario("Pop from non-empty stack") {
      Given("stack has 2 variables (G,H)")
      val stack=ForStack.empty
        .push(Variable("G"),ForState(Variable("G"),LineNumber(1)))
        .push(Variable("H"),ForState(Variable("H"),LineNumber(2)))
      assert(stack.lineFor(Variable("G")).contains(ForState(Variable("G"),LineNumber(1))))
      assert(stack.lineFor(Variable("H")).contains(ForState(Variable("H"),LineNumber(2))))
      When("variable G is popped from stack")
      val newStack=stack.pop(Variable("G"))
      Then("stack contains line 2 for vatiable H")
      assert(newStack.lineFor(Variable("H")).contains(ForState(Variable("H"),LineNumber(2))))
      And("stack does not contain line for variable G")
      assert(newStack.lineFor(Variable("H")).contains(ForState(Variable("H"),LineNumber(2))))
      And("after removal of variable H the stack is empty")
      val emptyStack=newStack.pop(Variable("H"))
      assert(emptyStack.isEmpty)
    }
    Scenario("Pop from empty stack") {
      Given("stack is empty")
      val stack=ForStack.empty
      assert(stack.isEmpty)
      When("a non-existing variable is removed from stack")
      val newStack=stack.pop(Variable("A"))
      Then("stack is still empty")
      assert(newStack.isEmpty)
    }
    Scenario("Pop of non-existing variable from non-empty stack") {
      Given("stack is not empty")
      val stack=ForStack.empty.push(Variable("W"),ForState(Variable("W"),LineNumber(20)))
      assert(stack.lineFor(Variable("W")).contains(ForState(Variable("W"),LineNumber(20))))
      When("a non-existing variable is checked")
      Then("stack returns empty value")
      assert(stack.lineFor(Variable("X")).isEmpty)
    }
    Scenario("Find nearest entry before given line") {
      Given("stack is not empty")
      val stack=ForStack.empty
        .push(Variable("W"),ForState(Variable("W"),LineNumber(20)))
        .push(Variable("X"),ForState(Variable("X"),LineNumber(25)))
        .push(Variable("Y"),ForState(Variable("Y"),LineNumber(30)))
      When("a variable before given line is checked")
      Then("stack returns proper variable")
      assert(stack.lineFor(LineNumber(27)).contains(ForState(Variable("X"),LineNumber(25))))
    }
  }
}
